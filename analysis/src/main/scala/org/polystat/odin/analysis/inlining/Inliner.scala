package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.syntax.parallel._
import cats.data.EitherNel
import cats.syntax.either._
import cats.data.{NonEmptyList => Nel}
import scala.annotation.tailrec

object Inliner {

  def inlineAllCalls(
    prog: EOProg[EOExprOnly]
  ): EitherNel[String, EOProg[EOExprOnly]] =
    prog
      .bnds
      .parTraverse(bnd =>
        LocateMethods.parseObject(bnd) match {
          case Some(objInfo) => rebuildObject(objInfo)
          case None => Right(bnd)
        }
      )
      .map(bnds => prog.copy(bnds = bnds))

  def generateLocalAttrsObj(
    newNameWithoutCollisions: String,
    nonPhiBnds: Vector[EOBndExpr[EOExprOnly]]
  ): EOBndExpr[EOExprOnly] = ???

  def inlineCalls(
    availableMethods: Map[EONamedBnd, MethodInfo],
    methodNameWhereToInline: EONamedBnd
  ): EitherNel[String, EOBndExpr[EOExprOnly]] = {
    val methodWhereInliningHappens = availableMethods(methodNameWhereToInline)

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

    val methodBody = methodWhereInliningHappens
      .calls
      .foldLeft[EitherNel[String, EOObj[EOExprOnly]]](
        Right(methodWhereInliningHappens.body)
      ) { case (oldMethodBody, call) =>
        for {
          // Checking that the desired method indeed exists
          methodToInlineInfo <-
            Either.fromOption(
              availableMethods.get(EOAnyNameBnd(LazyName(call.methodName))),
              Nel.one(
                s"Attempt to call non-existent method ${call.methodName}"
              )
            )

          // Checking that the call has the same amount of args as the method
          // needs
          _ <-
            if (methodToInlineInfo.body.freeAttrs.length == call.args.length)
              Right(())
            else Left(
              Nel.one(
                s"Wrong number of arguments given for method ${call.methodName}."
              )
            )

          // Propagate call arguments into method body
          // Generate a name for the localAttrs object
          // Extract local attributes into the localAttrs object
          // Extract the phi-Attribute
          // Rewire references to localAttributes to the localAttrs object
          // Inject localAttrs object if necessary
          // Substitute the call expression with phi-expression

          phiExpr <- Either.fromOption(
            methodToInlineInfo.body.bndAttrs.collectFirst {
              case EOBndExpr(EODecoration, phiExpr) => phiExpr
            },
            Nel.one(
              s"Method ${call.methodName} has no phi attribute"
            )
          )

          nonPhiBnds = methodToInlineInfo.body.bndAttrs.filter {
            case EOBndExpr(EODecoration, _) => false
            case _ => true
          }

          newMethodBodyWithModifiedCallSite =
            if (nonPhiBnds.nonEmpty) {
              oldMethodBody.flatMap(body =>
                Either.fromOption(
                  call
                    .callSite
                    .modifyOption(callsite => {
                      val newNameWithoutCollisions =
                        resolveNameCollisionsForLocalAttrObj(callsite)(
                          call.methodName
                        )
                      val localAttrsObj: EOBndExpr[EOExprOnly] =
                        generateLocalAttrsObj(
                          newNameWithoutCollisions,
                          nonPhiBnds
                        )

                      callsite.copy(
                        bndAttrs = callsite.bndAttrs.prepended(localAttrsObj)
                      )
                    })(body),
                  Nel.one(s"Could not modify callsite for ${call.methodName}.")
                )
              )
            } else
              oldMethodBody

          // replaceParams and replaceArgs for the method

          // Replacing the call itself
          inliningResult: EOExpr[EOExprOnly] = ???
          callPosition = call.callSite.andThen(call.callLocation)

          result <- newMethodBodyWithModifiedCallSite.flatMap(oldBody =>
            Either.fromOption(
              callPosition
                .replaceOption(Fix[EOExpr](inliningResult))(oldBody),
              Nel.one(
                s"Could not inline method ${call.methodName}. Possibly due to error with optics."
              )
            )
          )

        } yield result
      }

    methodBody.map(b => EOBndExpr(methodNameWhereToInline, Fix(b)))
  }

  def rebuildObject(obj: Object): EitherNel[String, EOBndExpr[EOExprOnly]] = {

    val newBinds: EitherNel[String, Vector[EOBndExpr[EOExprOnly]]] = obj
      .bnds
      .parTraverse {
        case Left(methodName) => inlineCalls(obj.methods, methodName)
        case Right(value) => Right(value)
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

}
