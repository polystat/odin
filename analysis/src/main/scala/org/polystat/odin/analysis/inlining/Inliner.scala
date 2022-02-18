package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.syntax.parallel._
import cats.data.EitherNel
import cats.syntax.either._
import cats.data.{NonEmptyList => Nel}
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.analysis.inlining.Optics.lenses._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps
import org.polystat.odin.backend.eolang.ToEO.instances._

import scala.annotation.{tailrec, unused}

object Inliner {

  /* The following steps are performed here
   * 0. All objects are extracted from the AST
   * 1. Every AST-object is converted into the 'Object' by *parseIfObject*
   * 2. Each created 'Object' is processed by *rebuildObject*
   * 2.1 All calls in the object are processed by *inlineCalls*, while non-call
   * binds are returned as is
   * 2.2 *propagateArguments* propagates arguments into the method body
   * 2.3 *generateLocalAttrsObj* creates the attrsObj of the method if necessary
   * 2.4 *inlineCalls* processes the phi-expression of the method
   * 2.5 *inlineCalls* injects the attrsObj in the callsite if necessary
   * 2.6 *inlineCalls* replaces the call with the corresponding phi-expression
   * 3. Successfully processed 'Object's are converted back into AST-objects
   * 4. Either the errors or the rebuilt AST are returned */

  def inlineAllCalls(
    prog: EOProg[EOExprOnly]
  ): EitherNel[String, EOProg[EOExprOnly]] =
    prog
      .bnds
      .parTraverse(bnd =>
        LocateMethods.parseObject(bnd, 0) match {
          case Some(objInfo) =>
            rebuildObject(objInfo)
          case None => Right(bnd)
        }
      )
      .map(bnds => prog.copy(bnds = bnds))

  def traverseExprWith(
    f: BigInt => EOExprOnly => Option[EOExprOnly],
    initialDepth: BigInt = 0
  )(initialExpr: EOExprOnly): EOExprOnly = {

    def recurse(depth: BigInt)(
      subExpr: EOExprOnly
    ): EOExprOnly = {

      def processExpr(
        fixedExpr: EOExprOnly
      ): EOExprOnly =
        f(depth)(fixedExpr) match {
          case Some(value) => value
          case None => fixedExpr
        }

      val res = Fix.un(subExpr) match {
        case copy: EOCopy[EOExprOnly] =>
          focusCopyTrg
            .modify(recurse(depth))
            .andThen(
              focusCopyArgs
                .modify(
                  _.map(
                    focusFromBndToExpr
                      .modify(recurse(depth))
                  )
                )
            )(copy)

        case obj: EOObj[EOExprOnly] =>
          focusFromEOObjToBndAttrs
            .modify(
              _.map(
                focusFromBndExprToExpr
                  .modify(
                    processExpr
                  )
                  .andThen(focusFromBndExprToExpr.modify(recurse(depth + 1)))
              )
            )(obj)

        case dot: EODot[EOExprOnly] =>
          focusDotSrc.modify(recurse(depth))(dot)

        case array: EOArray[EOExprOnly] =>
          focusArrayElems
            .modify(_.map(focusFromBndToExpr.modify(recurse(depth))))(array)

        case other => Fix.un(processExpr(Fix(other)))
      }

      Fix(res)
    }

    recurse(initialDepth)(initialExpr)
  }

  def propagateArguments(
    methodInfo: MethodInfo,
    call: Call,
  ): EOObj[EOExprOnly] = {
    val argMap =
      methodInfo.body.freeAttrs.map(_.name).zip(call.args).toMap

    val localNames = methodInfo
      .body
      .bndAttrs
      .filter {
        case EOBndExpr(EODecoration, _) => false
        case _ => true
      }
      .map(_.bndName.name.name)

    def incLocatorsBy1(
      app: EOExprOnly
    ): EOExprOnly = app match {
      case Fix(EOSimpleAppWithLocator(name, locator)) =>
        Fix(
          EOSimpleAppWithLocator[EOExprOnly](name, locator + 1)
        )
      case other => other
    }

    def incLocators(depth: BigInt)(
      app: EOExprOnly
    ): Option[EOExprOnly] = app match {
      case Fix(EOSimpleAppWithLocator(name, locator)) =>
        Some(
          Fix(
            EOSimpleAppWithLocator[EOExprOnly](
              name,
              locator + depth // + call.depth
            )
          )
        )
      case other => Some(other)
    }

    def isNotALocalAttr(
      depth: BigInt,
      app: EOSimpleAppWithLocator[EOExprOnly]
    ): Boolean = {
      !(localNames.contains(app.name) && app.locator == depth)
    }

    def incLocatorsIfNonPhi(@unused depth: BigInt)(
      app: EOExprOnly
    ): EOExprOnly = app match {
      case Fix(app @ EOSimpleAppWithLocator(name, locator))
           if isNotALocalAttr(depth, app) =>
        Fix(
          EOSimpleAppWithLocator[EOExprOnly](name, locator + 1 + call.depth)
        )
      case other => other
    }

    def propagateArguments(is_phi: Boolean)(
      currentDepth: BigInt
    )(app: EOExprOnly): Option[EOExprOnly] = {
      val appWithArgsPropagated = app match {
        case EOSimpleAppWithLocator(name, locator) if locator == currentDepth =>
          for {
            newExpr <- argMap.get(name).map(_.expr)
          } yield traverseExprWith(incLocators, currentDepth)(newExpr)

        case _ => None
      }

      // Adjusting locators for localAttrsObj
      if (is_phi)
        appWithArgsPropagated
      else
        appWithArgsPropagated
          // Increasing by one allows to account for the attrsObj
          .map(incLocatorsBy1)
          // Increasing by call.depth and 1 allows to account for non-args
          .orElse(app match {
            case aboba @ Fix(EOSimpleAppWithLocator(_, _)) =>
              Some(incLocatorsIfNonPhi(currentDepth)(aboba))
            case _ => None
          })

    }

    val newBnds = methodInfo
      .body
      .bndAttrs
      .map(bnd => {
        val is_phi = bnd match {
          case EOBndExpr(EODecoration, _) => true
          case EOBndExpr(_, _) => false
        }

        bnd.copy(
          expr = traverseExprWith(propagateArguments(is_phi))(bnd.expr)
        )
      })

    methodInfo
      .body
      .copy(
        bndAttrs = newBnds
      )
    //    focusFromEOObjToBndAttrs.modify(bnds =>
    //      bnds.map(
    // focusFromBndExprToExpr.modify(traverseExprWith(propagateArguments))
    //      )
    //    )(methodInfo.body)

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
    ): Option[EOExprOnly] = {
      val attrMap = attrsObjName.flatMap { objName =>
        {
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

      attrMap.flatMap(_.get(Fix.un(app))).map(a => Fix(a))
    }

    Fix.un(traverseExprWith(makeAppPointToAttrsObjIfNecessary)(Fix(phiExpr)))
  }

  def resolveNameCollisionsForLocalAttrObj(callsite: EOObj[EOExprOnly])(
    methodName: String
  ): String = {
    val usedNames = callsite.bndAttrs.collect {
      case EOBndExpr(EOAnyNameBnd(bnd: BndName), _) =>
        bnd.name
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
        def checkThatCalledMethodExists: Either[Nel[String], MethodInfo] =
          Either.fromOption(
            availableMethods.get(EOAnyNameBnd(LazyName(call.methodName))),
            Nel.one(
              s"Attempt to call non-existent method ${call.methodName}"
            )
          )

        def checkThatTheAmountOfArgsIsCorrect(
          methodToInlineInfo: MethodInfo
        ): Either[Nel[String], Unit] =
          if (methodToInlineInfo.body.freeAttrs.length == call.args.length)
            Right(())
          else Left(
            Nel.one(
              s"Wrong number of arguments given for method ${call.methodName}."
            )
          )

        def attemptToExtractPhiExpr(
          methodBody: EOObj[EOExprOnly]
        ): Either[Nel[String], EOExpr[EOExprOnly]] = Either.fromOption(
          methodBody.bndAttrs.collectFirst {
            case EOBndExpr(EODecoration, Fix(phiExpr)) => phiExpr
          },
          Nel.one(
            s"Method ${call.methodName} has no phi attribute"
          )
        )

        def getCallsite: Either[Nel[String], EOObj[EOExprOnly]] =
          currentMethodBodyWhereInliningHappens match {
            case err @ Left(_) => err
            case Right(body) => Either.fromOption(
                call
                  .callSite
                  .getOption(body),
                Nel.one(
                  s"Could not locate callsite for ${call.methodName}. Possibly due to a problem with optics."
                )
              )
          }

        def addAttrsObjToCallSiteIfNecessary(
          attrsObj: Option[EOBndExpr[EOExprOnly]],
          callSite: EOObj[EOExprOnly]
        ): Either[Nel[String], EOObj[EOExprOnly]] = {
          attrsObj
            .map(localAttrsObj =>
              Either.fromOption(
                call
                  .callSite
                  .modifyOption(callsite => {
                    callsite.copy(
                      bndAttrs = callsite.bndAttrs.prepended(localAttrsObj)
                    )
                  })(callSite),
                Nel
                  .one(s"Could not modify callsite for ${call.methodName}.")
              )
            )
            // No localAttrsObj needed => leave the body as is
            .getOrElse(Right(callSite))
        }

        for {
          methodToInlineInfo <- checkThatCalledMethodExists
          _ <- checkThatTheAmountOfArgsIsCorrect(methodToInlineInfo)

          // Propagate call arguments into method body
          // Generate a name for the localAttrs object
          // Extract local attributes into the localAttrs object
          // Extract the phi-Attribute
          // Rewire references to localAttributes to the localAttrs object
          // Inject localAttrs object if necessary
          // Substitute the call expression with phi-expression

          methodBodyWithArgsInlined =
            propagateArguments(methodToInlineInfo, call)

          phiExpr <- attemptToExtractPhiExpr(methodBodyWithArgsInlined)
          nonPhiBnds = methodBodyWithArgsInlined.bndAttrs.filter {
            case EOBndExpr(EODecoration, _) => false
            case _ => true
          }

          // If no localAttrsObj is needed => None
          attrsObj <-
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

          oldMethodBody <- currentMethodBodyWhereInliningHappens

          newMethodBodyPossiblyWithAttrsObj <-
            addAttrsObjToCallSiteIfNecessary(
              attrsObj,
              oldMethodBody
            )

          inliningResult = processPhi(phiExpr, attrsObj)

          callPosition = call.callSite.andThen(call.callLocation)
          result <-
            Either.fromOption(
              callPosition
                .replaceOption(Fix[EOExpr](inliningResult))(
                  newMethodBodyPossiblyWithAttrsObj
                ),
              Nel.one(
                s"Could not inline method ${call.methodName}. Possibly due to error with optics."
              )
            )

        } yield result
      }

    methodBody.map(body => EOBndExpr(methodNameWhereToInline, Fix(body)))
  }

  def rebuildObject(obj: Object): EitherNel[String, EOBndExpr[EOExprOnly]] = {

    val newBinds: EitherNel[String, Vector[EOBndExpr[EOExprOnly]]] = obj
      .bnds
      .parTraverse {
        case MethodPlaceholder(methodName) =>
          inlineCalls(obj.methods, methodName)
        case ObjectPlaceholder(objName) => Either
            .fromOption(
              obj.nestedObjects.get(objName),
              Nel.one(s"""
                         |Object with name ${objName.name.name} 
                         |can not be found directly in ${obj.name.name}. 
                         |This is most probably a programming error, report to developers.
                         | """.stripMargin)
            )
            .flatMap(rebuildObject)
        case BndItself(value) => Right(value)
      }

    newBinds.map { bnds =>
      val objExpr = EOObj(
        Vector(),
        None,
        bndAttrs = bnds
      )

      EOBndExpr(obj.name, Fix(objExpr))
    }
  }

  def main(args: Array[String]): Unit = {

    val code: String =
      """[] > a
        |  [self y] > x
        |    y > @
        |
        |  [self x y] > f
        |    self.g self x > h
        |    [] > @
        |      self.g self y > z
        |
        |  [self z] > g
        |    x > k
        |    z > l
        |    [] > @
        |      l > a
        |      k > b
        |      z > c
        |      self > d
        |""".stripMargin

    Parser
      .parse(code)
      .map(setLocators) match {
      case Left(value) => println(value)
      case Right(value) => inlineAllCalls(value) match {
          case Left(value) => println(value)
          case Right(value) => println(value.toEOPretty)
        }
    }

  }

}
