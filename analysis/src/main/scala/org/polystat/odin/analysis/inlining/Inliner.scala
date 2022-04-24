package org.polystat.odin.analysis.inlining


import cats.data.{EitherNel, NonEmptyList => Nel}
import cats.syntax.align._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.functor._
import cats.syntax.parallel._
import higherkindness.droste.data.Fix
import monocle.macros.GenLens
import monocle.{Iso, Lens, Optional}
import org.polystat.odin.analysis.inlining.Abstract.modifyExpr
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.analysis.inlining.LocateMethods._
import org.polystat.odin.analysis.inlining.Optics._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import scala.annotation.tailrec

object Inliner {

  /* The following steps are performed here
   * 0. All objects are extracted from the AST
   * 1. Every AST-object is converted into the 'Object' by *parseIfObject*
   * 2. Each created 'Object' is processed by *rebuildObject*
   * 2.1 All calls in the object are processed by *inlineCalls*, while non-call
   * binds are returned as is
   * 2.2 *propagateArguments* propagates arguments into the method body
   * 2.3 *inlineCalls* processes the phi-expression of the method
   * 2.4 *inlineCalls* injects the attrsObj in the callsite if necessary
   * 2.5 *inlineCalls* replaces the call with the corresponding phi-expression
   * 3. Successfully processed 'Object's are converted back into AST-objects
   * 4. Either the errors or the rebuilt AST are returned */
  type PartialObjectTree = ObjectTree[ObjectInfo[ParentName, MethodInfo]]
  type CompleteObjectTree = ObjectTree[ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]]
  type ObjectTreeForInlining = ObjectTree[ObjectInfo[ParentInfoForInlining[MethodInfo], MethodInfo]]
  type ObjectTreeForAnalysis = ObjectTree[
    ObjectInfoForAnalysis[
      ParentInfo[MethodInfo, ObjectInfoForAnalysis],
      MethodInfo
    ]
  ]

  def focusObjTreeChildren[
    P <: GenericParentInfo,
    M <: GenericMethodInfo,
    O[_ <: GenericParentInfo, _ <: GenericMethodInfo] <: GenericObjectInfo[_, _, O]
  ]: Lens[ObjectTree[O[P, M]], Map[EONamedBnd, ObjectTree[O[P, M]]]] =
    GenLens[ObjectTree[O[P, M]]](_.children)


  def inlineAllCalls(
                      prog: EOProg[EOExprOnly]
                    ): EitherNel[String, EOProg[EOExprOnly]] = {

    type ToplevelBndInfo = (
      Vector[Either[EOBnd[EOExprOnly], ObjectPlaceholder]],
        Map[EONamedBnd, PartialObjectTree],
      )
    def parseTopLevelBnds(
    bnds: Vector[EOBnd[EOExprOnly]]
    ): ToplevelBndInfo
     =
      bnds.foldLeft[ToplevelBndInfo]((Vector(), Map())) {
        case ((bnds, objs), bnd) => parseObject(bnd, 0, List()) match {
          case Some(value) => (
            bnds.appended(ObjectPlaceholder(value.info.name).asRight),
            objs.updated(value.info.name, value)
          )
          case None => (
            bnds.appended(bnd.asLeft),
            objs
          )
        }
      }

    for {
      progWithLocators <- setLocators(prog)
      (bnds, tree) <- parseTopLevelBnds(progWithLocators.bnds).asRight[Nel[String]]
      treeWithParents <- resolveParents(tree)
      inlinedObjs <- resolveParentsForInlining(treeWithParents).toVector.parTraverse {
        case (name, tree) => rebuildObject(tree).map((name, _))
      }
        .map(_.toMap)
      inlinedBnds = bnds.map {
        case Left(bnd) => bnd
        case Right(ObjectPlaceholder(name)) => inlinedObjs(name)
      }
    } yield prog.copy(bnds = inlinedBnds)
  }

  def createObjectTree(
                        prog: EOProg[EOExprOnly]
                      ): EitherNel[String, Map[EONamedBnd, PartialObjectTree]] = {
    setLocators(prog).map(
      _.bnds
        .flatMap(bnd =>
          parseObject(bnd, 0, List()).map(obj =>
            (obj.info.name, obj)
          )
        )
        .toMap
    )
  }

  def zipWithInlinedMethod[P <: GenericParentInfo](
                                                    obj: ObjectTree[ObjectInfo[P, MethodInfo]]
                                                  ): EitherNel[String, ObjectTree[ObjectInfo[P, MethodInfoAfterInlining]]] = {
    obj.traverseMethods[P, MethodInfo, EitherNel[String, *], MethodInfoAfterInlining, ObjectInfo](
      (name, curMethod, curObj) =>
        inlineCalls(curObj.methods, name).map(bodyAfter =>
          MethodInfoAfterInlining(
            body = curMethod.body,
            bodyAfterInlining = bodyAfter
          )
        )
    )
  }

  def resolveParents(
                      objs: Map[EONamedBnd, PartialObjectTree]
                    ): EitherNel[String, Map[EONamedBnd, CompleteObjectTree]] = {

    type Context = Map[EONamedBnd, PartialObjectTree]
    type PathToContext = Optional[Context, Context]

    def retrieveParentInfo(ctxs: Nel[PathToContext])(info: Option[ParentName]): EitherNel[String, Option[ParentInfo[MethodInfo, ObjectInfo]]] = {
      info match {
        case Some(info@ParentName(ObjectNameWithLocator(locator, name))) =>
          val names = name.names
          val pathFromCtxToObj =
            names.map(name => optionals.mapValueAtKey[EONamedBnd, PartialObjectTree](EOAnyNameBnd(LazyName(name))))
              .reduceLeft((acc, next) => acc.andThen(focusObjTreeChildren[ParentName, MethodInfo, ObjectInfo]).andThen(next))
          val pathToObject = ctxs.toList.lift(locator.toInt).toRight(Nel.one(
            s"Locator overshoot at ${info.toEOName}"
          )).map(_.andThen(pathFromCtxToObj))

          for {
            parentGetter <- pathToObject
            _ <- parentGetter.getOption(objs).toRight(
              Nel.one(s"There is no such parent with name \"${info.toEOName}\".")
            )
          } yield Some(ParentInfo(
            linkToParent = parentGetter.asInstanceOf[
              Optional[
                Map[EONamedBnd, CompleteObjectTree],
                CompleteObjectTree]
            ]
          ))
        case None => Right(None)
      }

    }

    def recurse(cur: PartialObjectTree)(ctxs: Nel[PathToContext]): EitherNel[String, CompleteObjectTree] = {
      val parentName = cur.info.parentInfo
      (
        retrieveParentInfo(ctxs)(parentName),
        cur.children.toList.parTraverse {
          case (name, subTree) =>
            val ctx =
              ctxs
                .head
                .andThen(optionals.mapValueAtKey[EONamedBnd, PartialObjectTree](name))
                .andThen(focusObjTreeChildren[ParentName, MethodInfo, ObjectInfo])
            recurse(subTree)(ctxs.prepend(ctx)).map((name, _))
        }.map(_.toMap)

        )
        .mapN((info, children) =>
          ObjectTree(
            cur.info.copy(parentInfo = info),
            children
          )
        )
    }


    objs.toList.parTraverse {
      case (name, tree) => recurse(tree)(
        Nel(
          optionals.mapValueAtKey(name).andThen(focusObjTreeChildren),
          List(Iso.id)
        )
      ).map((name, _))
    }.map(_.toMap)
  }

  def accumulateMethods(fullTree: Map[EONamedBnd, CompleteObjectTree])(cur: CompleteObjectTree): Map[EONamedBnd, MethodInfo] = {
    val parent = cur.info.parentInfo.flatMap(_.linkToParent.getOption(fullTree))
    val parentMethods: Map[EONamedBnd, MethodInfo] = parent match {
      case Some(parent) => accumulateMethods(fullTree)(parent)
      case None => Map()
    }

    parentMethods.concat(cur.info.methods)
  }


  def resolveIndirectMethods(
                              objs: Map[EONamedBnd, CompleteObjectTree]
                            ): EitherNel[String, Map[EONamedBnd, ObjectTreeForAnalysis]] = {


    def recurse(cur: CompleteObjectTree): EitherNel[String, ObjectTreeForAnalysis] = {
      val allMethods = accumulateMethods(objs)(cur)
      (
        Right(allMethods.removedAll(cur.info.methods.keySet)),
        cur.children.toList.parTraverse {
          case (name, subTree) =>
            recurse(subTree).map((name, _))
        }.map(_.toMap)

        )
        .mapN((methods, children) =>
          ObjectTree(
            ObjectInfoForAnalysis(
              name = cur.info.name,
              methods = cur.info.methods,
              parentInfo = cur.info.parentInfo.map(_.asInstanceOf[ParentInfo[MethodInfo, ObjectInfoForAnalysis]]),
              indirectMethods = methods.map {
                case (name, info) => (name, MethodInfoForAnalysis(body = info.body, depth = info.depth))
              },
              allMethods = allMethods.map {
                case (name, info) => (name, MethodInfoForAnalysis(body = info.body, depth = info.depth))
              }
            ),
            children
          )
        )
    }


    objs.toList.parTraverse {
      case (name, tree) => recurse(tree).map((name, _))
    }.map(_.toMap)


  }

  def resolveParentsForInlining(objs: Map[EONamedBnd, CompleteObjectTree]): Map[EONamedBnd, ObjectTreeForInlining] = {
    def recurse(currentLevel: Map[EONamedBnd, CompleteObjectTree]): Map[EONamedBnd, ObjectTreeForInlining] = {
      currentLevel.fmap {
        subTree =>
          ObjectTree(
            info = subTree.info.copy(
              parentInfo = subTree.info.parentInfo.map(_ =>
                ParentInfoForInlining(accumulateMethods(objs)(subTree))
              )
            ),
            children = recurse(subTree.children)
          )
      }
    }

    recurse(objs)

  }

  type AnalysisInfo = ObjectInfoForAnalysis[ParentInfo[MethodInfo, ObjectInfoForAnalysis], MethodInfo]

  def zipMethodsWithTheirInlinedVersionsFromParent(prog: EOProg[EOExprOnly]): EitherNel[
    String,
    Map[
      EONamedBnd,
      ObjectTree[
        (AnalysisInfo, AnalysisInfo)
      ]
    ]
  ] = {
    val methods = for {
      tree <- createObjectTree(prog)
      treeWithParents <- resolveParents(tree)
      treeWithIndirectMethods <- resolveIndirectMethods(treeWithParents)
    } yield treeWithIndirectMethods

    val methodsAfterInlining = for {
      inlined <- inlineAllCalls(prog)
      tree <- createObjectTree(inlined)
      treeWithParents <- resolveParents(tree)
      treeWithIndirectMethods <- resolveIndirectMethods(treeWithParents)
    } yield treeWithIndirectMethods

    for {
      before <- methods
      after <- methodsAfterInlining
    } yield before.alignWith(after)(_.onlyBoth.get match {
      case (before, after) => before.zip(after)
    })
  }

  def incLocatorBy(n: BigInt)(
    app: EOExprOnly
  ): EOExprOnly = app match {
    case Fix(EOSimpleAppWithLocator(name, locator)) =>
      Fix[EOExpr](
        EOSimpleAppWithLocator(name, locator + n)
      )
    case other => other
  }

  def propagateArguments(
                          methodInfo: MethodInfo,
                          call: Call,
                        ): EOObj[EOExprOnly] = {
    val argMap = methodInfo.body.freeAttrs.map(_.name).zip(call.args).toMap
    val localNames = methodInfo
      .body
      .bndAttrs
      .filter {
        case EOBndExpr(EODecoration, _) => false
        case _ => true
      }
      .map(_.bndName.name.name)

    def getAppReplacementFromArgMap(
                                     currentDepth: BigInt
                                   ): EOExprOnly => Option[EOExprOnly] = {
      case EOSimpleAppWithLocator(name, locator) if locator == currentDepth =>
        argMap
          .get(name)
          .map(_.expr)
      case _ => None
    }

    def propagateArguments(
                            currentDepth: BigInt
                          ): EOExprOnly => EOExprOnly = {
      // It is an argument
      case app@Fix(EOSimpleAppWithLocator(name, locator))
        if locator == currentDepth =>
        argMap
          .get(name)
          // Increase all locators in the expression by their depth
          // Add +1 to account for the additional nestedness of attrsObj
          .map(replacement =>
            modifyExpr(incLocatorBy, currentDepth + 1)(replacement.expr)
          )
          // If it points to the method body, but is not an argument => ignore
          // it
          .getOrElse(app)

      // It is some other application
      case app@Fix(EOSimpleAppWithLocator(name, locator))
        // Checking that it does not point to a local attribute
        if !(localNames.contains(name) && locator == currentDepth) =>
        prisms
          .fixToEOSimpleAppWithLocator
          .andThen(
            lenses.focusFromEOSimpleAppWithLocatorToLocator
          )
          .modify(_ + 1 + call.depth)(app)

      // Non-application
      case other => other
    }

    def processPhi(currentDepth: BigInt)(app: EOExprOnly): EOExprOnly = {
      getAppReplacementFromArgMap(currentDepth)(app)
        .map(replacement => modifyExpr(incLocatorBy, currentDepth)(replacement))
        .getOrElse(app)
    }

    val newBnds = methodInfo
      .body
      .bndAttrs
      .map(bnd => {
        val newExpr = bnd match {
          case EOBndExpr(EODecoration, expr) =>
            modifyExpr(processPhi)(expr)
          case EOBndExpr(_, expr) =>
            modifyExpr(propagateArguments)(expr)
        }

        bnd.copy(
          expr = newExpr
        )
      })

    methodInfo
      .body
      .copy(
        bndAttrs = newBnds
      )
  }

  def processPhi(
                  phiExpr: EOExpr[EOExprOnly],
                  attrsObj: Option[EOBndExpr[EOExprOnly]]
                ): EOExpr[EOExprOnly] = {

    val attrsObjName = attrsObj.map(_.bndName.name.name)
    val availableBndNames =
      attrsObj.collect { case EOBndExpr(_, Fix(EOObj(_, _, bndAttrs))) =>
        bndAttrs.map(_.bndName.name.name)
      }

    def makeAppPointToAttrsObjIfNecessary(depth: BigInt)(
      app: EOExprOnly
    ): EOExprOnly = {
      val attrMap = attrsObjName.flatMap { objName => {
        availableBndNames.map { names =>
          // Applications that are expected to refer to the attrsObj
          val apps =
            names.map(name => EOSimpleAppWithLocator(name, depth))
          // Application properly referring to the attrsObj
          val appsToAttrsObj = apps.map(app =>
            EODot(
              Fix[EOExpr](EOSimpleAppWithLocator(objName, depth)),
              app.name
            )
          )
          // Making application correspond to the proper reference to attrsObj
          apps
            .zip(appsToAttrsObj)
            .toMap[EOExpr[EOExprOnly], EODot[EOExprOnly]]
        }
      }
      }

      attrMap
        .flatMap(_.get(Fix.un(app)))
        .map(Fix(_))
        .getOrElse(app)
    }

    Fix.un(modifyExpr(makeAppPointToAttrsObjIfNecessary)(Fix(phiExpr)))
  }

  def resolveNameCollisionsForLocalAttrObj(callsite: EOObj[EOExprOnly])(
    methodName: String
  ): String = {
    val usedNames = callsite.bndAttrs.collect {
      case EOBndExpr(EOAnyNameBnd(bnd: BndName), _) => bnd.name
    }

    @tailrec
    def rec(name: String): String =
      if (usedNames.contains(name)) rec(name + "1") else name

    rec(s"local_$methodName")
  }

  def inlineCalls(
                   availableMethods: Map[EONamedBnd, MethodInfo],
                   methodNameWhereToInline: EONamedBnd
                 ): EitherNel[String, EOBndExpr[EOExprOnly]] = {

    val methodWhereInliningHappens = availableMethods(methodNameWhereToInline)
    val methodBody = methodWhereInliningHappens
      .calls
      // Going over every call in the method and attempting to inline it
      .foldLeft[EitherNel[String, EOObj[EOExprOnly]]](
        // Starting point is the initial body of the method
        Right(methodWhereInliningHappens.body)
      ) { case (currentMethodBodyWhereInliningHappens, call) =>
        def checkThatCalledMethodExists: EitherNel[String, MethodInfo] =
          availableMethods
            .get(EOAnyNameBnd(LazyName(call.methodName)))
            .toRight(
              Nel.one(
                s"Inliner: attempt to call non-existent method \"${call.methodName}\""
              )
            )

        def checkThatTheAmountOfArgsIsCorrect(
                                               methodToInlineInfo: MethodInfo
                                             ): EitherNel[String, Unit] = {

          if (methodToInlineInfo.body.freeAttrs.length == call.args.length)
            Right(())
          else Left(
            Nel.one(
              s"Wrong number of arguments given for method ${call.methodName}."
            )
          )
        }

        def extractPhiExpr(
                            methodBody: EOObj[EOExprOnly]
                          ): EitherNel[String, EOExpr[EOExprOnly]] =
          methodBody
            .bndAttrs
            .collectFirst { case EOBndExpr(EODecoration, Fix(phiExpr)) =>
              phiExpr
            }
            .toRight(
              Nel.one(
                s"Method ${call.methodName} has no phi attribute"
              )
            )

        def getCallsite: EitherNel[String, EOObj[EOExprOnly]] =
          currentMethodBodyWhereInliningHappens match {
            case err@Left(_) => err
            case Right(body) =>
              call
                .callSite
                .getOption(body)
                .toRight(
                  Nel.one(
                    s"""Could not locate call-site for ${call.methodName}.
                       ||This is most probably a programming error, report to developers.
                       |""".stripMargin
                  )
                )
          }

        def addAttrsObjToCallSiteIfNecessary(
                                              attrsObj: Option[EOBndExpr[EOExprOnly]],
                                              callSite: EOObj[EOExprOnly]
                                            ): EitherNel[String, EOObj[EOExprOnly]] = {
          attrsObj
            .map(localAttrsObj =>
              call
                .callSite
                .modifyOption(callsite => {
                  callsite.copy(
                    bndAttrs = callsite.bndAttrs.prepended(localAttrsObj)
                  )
                })(callSite)
                .toRight(
                  Nel
                    .one(
                      s"""
                         |Could not modify call-site for ${call.methodName}.
                         |This is most probably a programming error, report to developers."""
                    )
                )
            )
            // No localAttrsObj needed => leave the body as is
            .getOrElse(Right(callSite))
        }

        def extractLocalAttrsObj(
                                  nonPhiBnds: Vector[EOBndExpr[EOExprOnly]]
                                ): EitherNel[String, Option[EOBndExpr[Fix[EOExpr]]]] = {
          if (nonPhiBnds.nonEmpty) {

            val attrsObjName = getCallsite.map(callsite =>
              resolveNameCollisionsForLocalAttrObj(callsite)(
                call.methodName
              )
            )

            attrsObjName
              .map(name => {
                val bndName = EOAnyNameBnd(LazyName(name))
                val obj = EOObj(Vector.empty, None, nonPhiBnds)

                EOBndExpr(bndName, Fix(obj))
              })
              .map(Some.apply)
          } else {
            Right(None)
          }
        }

        for {
          methodToInlineInfo <- checkThatCalledMethodExists
          _ <- checkThatTheAmountOfArgsIsCorrect(methodToInlineInfo)

          methodBodyWithArgsInlined =
            propagateArguments(methodToInlineInfo, call)
          phiExpr <- extractPhiExpr(methodBodyWithArgsInlined)
          nonPhiBnds = methodBodyWithArgsInlined.bndAttrs.filter {
            case EOBndExpr(EODecoration, _) => false
            case _ => true
          }
          attrsObj <- extractLocalAttrsObj(nonPhiBnds)
          oldMethodBody <- currentMethodBodyWhereInliningHappens

          newMethodBodyPossiblyWithAttrsObj <-
            addAttrsObjToCallSiteIfNecessary(
              attrsObj,
              oldMethodBody
            )
          inliningResult = processPhi(phiExpr, attrsObj)
          callPosition = call.callSite.andThen(call.callLocation)

          result <-
            callPosition
              .replaceOption(Fix[EOExpr](inliningResult))(
                newMethodBodyPossiblyWithAttrsObj
              )
              .toRight(
                Nel.one(
                  s"""
                     |Could not inline method ${call.methodName}.
                     |This is most probably a programming error, report to developers.
                     |""".stripMargin
                )
              )

        } yield result
      }

    methodBody.map(body => EOBndExpr(methodNameWhereToInline, Fix(body)))
  }

  def rebuildObject(obj: ObjectTreeForInlining): EitherNel[String, EOBndExpr[EOExprOnly]] = {

    val newBinds: EitherNel[String, Vector[EOBndExpr[EOExprOnly]]] = obj
      .info
      .bnds
      .parTraverse {
        case MethodPlaceholder(methodName) =>
          inlineCalls(
            obj.info.parentInfo.map(_.parentMethods).getOrElse(Map()).concat(obj.info.methods),
            methodName
          )
        case ObjectPlaceholder(objName) =>
          obj
            .children
            .get(objName)
            .toRight(
              Nel.one(
                s"""
                   |Object with name ${objName.name.name}
                   |can not be found directly in ${obj.info.name.name}.
                   |This is most probably a programming error, report to developers.
                   |""".stripMargin)
            )
            .flatMap(rebuildObject)
        case ParentPlaceholder(expr) => Right(EOBndExpr(EODecoration, expr))
        case BndItself(value) => Right(value)
      }

    newBinds.map { bnds =>
      val objExpr = EOObj(
        Vector(),
        None,
        bndAttrs = bnds
      )

      EOBndExpr(obj.info.name, Fix(objExpr))
    }
  }

}
