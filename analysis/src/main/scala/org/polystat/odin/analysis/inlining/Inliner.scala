package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.instances.progToEO
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps
import cats.syntax.traverse._

object Big {
  def unapply(n: BigInt): Option[Int] = Some(n.toInt)
}

object Inliner {
  case class Method(name: String, body: EOObj[Fix[EOExpr]])

  def extractMethods(obj: EOObj[Fix[EOExpr]]): List[Method] = {
    obj
      .bndAttrs
      .flatMap {
        case EOBndExpr(EOAnyNameBnd(LazyName(methodName)), Fix(expr)) =>
          expr match {
            case method @ EOObj(LazyName("self") +: _, _, _) =>
              Some(Method(methodName, method))
            case _ => None
          }
        case _ => None
      }
      .toList
  }

  def inlineCall(
    call: EOCopy[Fix[EOExpr]],
    availableMethods: List[Method]
  ): Either[String, EOExpr[Fix[EOExpr]]] = {
    def helper(value: EOCopy[Fix[EOExpr]]): EOExpr[Fix[EOExpr]] = ???

    call.trg match {
      case EODot(EOSimpleAppWithLocator("self", Big(0)), methodName) =>
        availableMethods
          .find(method => method.name == methodName)
          .map(method =>
            if (call.args.length == method.body.freeAttrs.length)
              Right(helper(call))
            else
              Left(s"Wrong number of arguments given for method $methodName.")
          )
          .getOrElse(Right(call))

      case _ => Right(call)
    }
  }

  def inlineCalls(
    prog: EOProg[Fix[EOExpr]]
  ): Either[String, EOProg[Fix[EOExpr]]] = {

    def exprHelper(
      availableMethods: List[Method]
    )(
      expr: EOExpr[Fix[EOExpr]]
    ): Either[String, EOExpr[Fix[EOExpr]]] = expr match {
      // Method
      case method @ EOObj(LazyName("self") +: _, _, bndAttrs) =>
        bndAttrs
          .traverse(bndExprHelper(availableMethods))
          .map(bnds => method.copy(bndAttrs = bnds))

      // Object
      case obj @ EOObj(_, _, bndAttrs) =>
        bndAttrs
          .traverse(bndExprHelper(extractMethods(obj)))
          .map(bnds => obj.copy(bndAttrs = bnds))

      case dot @ EODot(Fix(src), _) =>
        exprHelper(availableMethods)(src).map(src => dot.copy(src = Fix(src)))

      case call: EOCopy[Fix[EOExpr]] => inlineCall(call, availableMethods)

      case other => Right(other)
    }

    def bndExprHelper(
      availableMethods: List[Method]
    )(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): Either[String, EOBndExpr[Fix[EOExpr]]] =
      exprHelper(availableMethods)(Fix.un(bnd.expr)).map(expr =>
        bnd.copy(expr = Fix(expr))
      )

    def bndHelper(
      availableMethods: List[Method]
    )(
      bnd: EOBnd[Fix[EOExpr]]
    ): Either[String, EOBnd[Fix[EOExpr]]] =
      bnd match {
        case EOAnonExpr(Fix(expr)) =>
          exprHelper(availableMethods)(expr).map(value =>
            EOAnonExpr(Fix(value))
          )

        case bnd: EOBndExpr[Fix[EOExpr]] =>
          bndExprHelper(availableMethods)(bnd)
      }

    prog
      .bnds
      .traverse(bndHelper(List.empty))
      .map(bnds => prog.copy(bnds = bnds))
  }

  def main(args: Array[String]): Unit = {
    val code: String =
      """
        |
        |[] > outer
        |  [] > self
        |    256 > magic
        |    [] > dummy
        |      [self] > aboba
        |        22 > @
        |      [self outer] > innerMethod
        |        self.aboba self > @
        |      self.innerMethod self self > @
        |    self "yahoo" > @
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
