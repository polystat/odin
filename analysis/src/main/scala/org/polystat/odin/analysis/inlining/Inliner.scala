package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.syntax.parallel._
import cats.data.EitherNel
import cats.syntax.either._
import cats.data.{NonEmptyList => Nel}
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps
import org.polystat.odin.backend.eolang.ToEO.instances._

import scala.annotation.tailrec

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

  // TODO: properly implement the locator processing
  def generateLocalAttrsObj(
    objName: String,
    nonPhiBnds: Vector[EOBndExpr[EOExprOnly]]
  ): EOBndExpr[EOExprOnly] = {
    val name = EOAnyNameBnd(LazyName(objName))
    // process the bnds here...
    val obj = EOObj(Vector.empty, None, nonPhiBnds)

    EOBndExpr(name, Fix(obj))
  }

  // TODO: actually propagate arguments
  def propagateArguments(
    methodInfo: MethodInfo,
    call: Call
  ): EitherNel[String, EOObj[EOExprOnly]] = {
    println(call)

    Right(methodInfo.body)
  }

  // TODO: actually rewire references to attrsObj
  def processphi(
    phiExpr: EOExpr[EOExprOnly],
    attrsObj: Option[EOBndExpr[EOExprOnly]]
  ): EOExpr[EOExprOnly] = {
    val attrsObjName = attrsObj.map(_.bndName.name)
    println(s"Adapting call for $attrsObjName")

    phiExpr
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

          methodBodyWithArgsInlined <-
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
                .map(name =>
                  generateLocalAttrsObj(
                    name,
                    nonPhiBnds
                  )
                )
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

          inliningResult = processphi(phiExpr, attrsObj)

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
      """[] > outer
        |  256 > magic
        |  [] > dummy
        |    [self] > bMethod
        |      22 > @
        |    [self outer] > innerMethod
        |      [self] > innerInnerMethod
        |        ^.self.bMethod ^.self > @
        |      self.bMethod self > @
        |    $.innerMethod 1 1 > b
        |  self "yahoo" > @
        |  [self] > method
        |    self.magic > @
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
