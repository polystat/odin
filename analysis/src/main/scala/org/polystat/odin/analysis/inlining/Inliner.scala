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

  def inlineCall(
    call: EOCopy[Fix[EOExpr]],
    availableMethods: MethodList
  ): Either[String, EOExpr[Fix[EOExpr]]] = {
    def inline(
      call: EOCopy[Fix[EOExpr]],
      method: Method
    ): EOExpr[Fix[EOExpr]] = ???

    call.trg match {
      case EODot(_, methodName) =>
        availableMethods
          .methods
          .find(method => method.name == methodName)
          .map(method =>
            if (call.args.length == method.body.freeAttrs.length)
              Right(inline(call, method))
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
      availableMethods: MethodList,
      currentDepth: BigInt
    )(
      expr: EOExpr[Fix[EOExpr]]
    ): Either[String, EOExpr[Fix[EOExpr]]] = expr match {

      case method @ EOObj(LazyName("self") +: _, _, bndAttrs) =>
        bndAttrs
          .traverse(bndExprHelper(availableMethods, currentDepth + 1))
          .map(bnds => method.copy(bndAttrs = bnds))

      case obj @ EOObj(_, _, bndAttrs) =>
        val newDepth = currentDepth + 1
        val newMethods = extractMethods(obj, newDepth)

        bndAttrs
          .traverse(bndExprHelper(newMethods, newDepth))
          .map(bnds => obj.copy(bndAttrs = bnds))

      case dot @ EODot(Fix(src), _) =>
        exprHelper(availableMethods, currentDepth)(src).map(src =>
          dot.copy(src = Fix(src))
        )

      case call @ EOCopy(
             EODot(EOSimpleAppWithLocator("self", locator), _),
             _
           ) =>
        // Checking that locator refers to the right obj
        // -1 accounts for the additional depth of the method)
        if (availableMethods.depth == currentDepth - locator - 1)
          call.args.headOption match {
            case Some(EOAnonExpr(EOSimpleAppWithLocator("self", loc)))
                 if loc == locator => inlineCall(call, availableMethods)
            case _ => Right(call)
          }
        else Right(call)

      case other => Right(other)
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
