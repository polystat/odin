package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.core.ast.{EODecoration, _}
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.instances.progToEO
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps
import cats.syntax.traverse._

object Inliner {
  case class Method(name: String, body: EOObj[Fix[EOExpr]])
  case class MethodList(methods: List[Method], depth: BigInt)

  def extractMethods(
    obj: EOObj[Fix[EOExpr]],
    depth: BigInt
  ): MethodList = {
    val methods = obj
      .bndAttrs
      .flatMap {
        case EOBndExpr(EOAnyNameBnd(LazyName(methodName)), Fix(expr)) =>
          expr match {
            case method @ EOObj(LazyName("self") +: _, _, bndAttrs) =>
              bndAttrs
                .find {
                  case EOBndExpr(EODecoration, _) => true
                  case _ => false
                }
                .map(_ => Method(methodName, method))
            case _ => None
          }
        case _ => None
      }
      .toList

    MethodList(methods, depth)
  }

  //  def propagateArgs(
  //    call: EOCopy[Fix[EOExpr]],
  //    methodBody: EOObj[Fix[EOExpr]]
  //  ): EOExpr[Fix[EOExpr]] = ???
  //
  //  def splitMethodBody(
  //    body: Vector[EOBndExpr[Fix[EOExpr]]],
  //    usedNames: List[String]
  //  ): Vector[EOBndExpr[Fix[EOExpr]]] =
  //    ???

  def tryInlineCalls(
    availableMethods: MethodList,
    currentDepth: BigInt
  )(
    binds: Vector[EOBndExpr[Fix[EOExpr]]]
  ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {

    def checkCallValidity(
      bnd: EOBndExpr[Fix[EOExpr]],
      call: EOCopy[Fix[EOExpr]],
      locator: BigInt,
      method: Method
    ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {

      lazy val callHasProperNumOfArgs =
        call.args.length == method.body.freeAttrs.length

      // -1 accounts for the additional depth of the method
      lazy val callHasCorrectDepth =
        availableMethods.depth == currentDepth - locator - 1

      lazy val callHasCorrectSelfInArgs = call.args.headOption match {
        case Some(EOAnonExpr(EOSimpleAppWithLocator("self", loc)))
             if loc == locator => true
        case _ => false
      }

      lazy val phi = method
        .body
        .bndAttrs
        .find {
          case EOBndExpr(EODecoration, _) => true
          case _ => false
        }
        .map(bnd => Right(bnd))
        .getOrElse(
          Left(s"Method ${method.name} does not have a Phi attribute")
        )

      (
        callHasCorrectDepth,
        callHasCorrectSelfInArgs,
        callHasProperNumOfArgs
      ) match {
        // The bnd does not require inlining
        case (false, _, _) | (_, false, _) => Right(Vector(bnd))
        case (true, true, false) => Left(
            s"Wrong number of arguments given for method ${method.name}."
          )
        // The call matches all criteria -> needs inlining
        case (true, true, true) => phi.map(phi => {
            Vector(phi)
          })
      }

    }

    def processIfCall(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] =
      bnd match {
        case EOBndExpr(
               _,
               Fix(
                 call @ EOCopy(
                   EODot(EOSimpleAppWithLocator("self", locator), methodName),
                   _
                 )
               )
             ) =>
          availableMethods
            .methods
            .find(method => method.name == methodName)
            .map(method => checkCallValidity(bnd, call, locator, method))
            .getOrElse(
              Left(s"Attempt to call non-existent method $methodName.")
            )

        case other => Right(Vector(other))
      }

    val results = binds.map(processIfCall)
    val errors = results.collect { case err @ Left(_) => err }
    val successes = results.collect { case err @ Right(_) => err }

    errors.headOption match {
      case Some(error) => error
      case None => Right(successes.flatMap(_.value))
    }
  }

  def inlineCalls(
    prog: EOProg[Fix[EOExpr]]
  ): Either[String, EOProg[Fix[EOExpr]]] = {

    def exprHelper(
      availableMethods: MethodList,
      currentDepth: BigInt
    )(
      expr: EOExpr[Fix[EOExpr]]
    ): Either[String, EOExpr[Fix[EOExpr]]] = {
      lazy val newDepth = currentDepth + 1

      expr match {
        case method @ EOObj(LazyName("self") +: _, _, bndAttrs) =>
          bndAttrs
            .traverse(bndExprHelper(availableMethods, newDepth))
            .flatMap(tryInlineCalls(availableMethods, newDepth))
            .map(bnds => method.copy(bndAttrs = bnds))

        case obj @ EOObj(_, _, bndAttrs) =>
          val newMethods = extractMethods(obj, newDepth)

          bndAttrs
            .traverse(bndExprHelper(newMethods, newDepth))
            .flatMap(tryInlineCalls(availableMethods, newDepth))
            .map(bnds => obj.copy(bndAttrs = bnds))

        case dot @ EODot(Fix(src), _) =>
          exprHelper(availableMethods, currentDepth)(src).map(src =>
            dot.copy(src = Fix(src))
          )

        case other => Right(other)
      }
    }

    def bndExprHelper(
      availableMethods: MethodList,
      currentDepth: BigInt
    )(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): Either[String, EOBndExpr[Fix[EOExpr]]] =
      exprHelper(availableMethods, currentDepth)(Fix.un(bnd.expr)).map(expr =>
        bnd.copy(expr = Fix(expr))
      )

    def bndHelper(
      availableMethods: MethodList,
      currentDepth: BigInt
    )(
      bnd: EOBnd[Fix[EOExpr]]
    ): Either[String, EOBnd[Fix[EOExpr]]] =
      bnd match {
        case EOAnonExpr(Fix(expr)) =>
          exprHelper(availableMethods, currentDepth)(expr).map(value =>
            EOAnonExpr(Fix(value))
          )
        case bnd: EOBndExpr[Fix[EOExpr]] =>
          bndExprHelper(availableMethods, currentDepth)(bnd)
      }

    prog
      .bnds
      .traverse(bndHelper(MethodList(List.empty, 0), 0))
      .map(bnds => prog.copy(bnds = bnds))
  }

  def main(args: Array[String]): Unit = {

    val code: String =
      """
        |
        |[] > outer
        |  256 > magic
        |  [] > dummy
        |    [self] > bMethod
        |      22 > @
        |    [self outer] > innerMethod
        |      [self] > innerInnerMethod
        |        ^.self.bMethod ^.self > @
        |      self.bMethod self > @
        |
        |    $.innerMethod 1 1 > b
        |  self "yahoo" > @
        |  [self] > method
        |    self.magic > @
        |
        |""".stripMargin

    Parser
      .parse(code)
      .map(setLocators) match {
      case Left(value) => println(value)
      case Right(value) => inlineCalls(value) match {
          case Left(value) => println(value)
          case Right(value) => println(value.toEOPretty)
        }
    }
  }

}
