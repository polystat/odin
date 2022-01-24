package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.eo.Parser

// + alias aboba.hui.stdout
// [] > outer
//   [] > self
//     228 > magic
//     ^.^.stdout "ya hui" > @
//   [self] > method
//     # EODot(EODot(EODot(EOSimpleApp("^"), "^"), "self"), "magic") ->
//     #   EODot(EOSimpleAppWithLocator("self", 2), "magic")
//     ^.self.magic > @
//
// $.outer > wannabe-outer
//
// Context: Map[String, Int]
//
// 0. Resolve explicit locator chains (^.^.aboba) during parsing
// 1. Create the first context that includes names of all top-lvl EOObjs
// 1.1. Replace All top-level applications (a > b) with application of
// 0-locators ($.a > b)
//       [pending approval]
// 2. Recursively explore the AST
// 2.1 Upon meeting an EOSimpleAPP() -> resolve it using the current context
// into EOSimpleAppWithLocator()
// 2.2 Upon meeting a EOObj() -> use its contents to update the context and pass
// it in the next recursive call
//
//
//

object Context {

  type Context = Map[String, Int]

  def resolveLocator(
    ctx: Context,
    app: EOSimpleApp[Fix[EOExpr]]
  ): EOSimpleAppWithLocator[Fix[EOExpr]] = ???

  def rebuildContext(
    ctx: Context,
    depth: Int,
    objs: List[EOObj[Fix[EOExpr]]]
  ): Context = ???

  def traverseAST(code: EOProg[Fix[EOExpr]]): EOProg[Fix[EOExpr]] = {
    def exprHelper(expr: EOExpr[Fix[EOExpr]]): EOExpr[Fix[EOExpr]] =
      expr match {
        case EOObj(_, _, bndAttrs) => {
          ???
//          bndAttrs.flatMap(bndHelper).toList
        }
        case EOSimpleApp(_) => ???
        case EOCopy(t, args) => EoCopy(args.value.map(bndHelper))

        case _ => ???
      }

    def bndHelper(bnd: EOBnd[Fix[EOExpr]]): EOExpr[Fix[EOExpr]] =
      bnd match {
        case EOAnonExpr(Fix(expr)) => exprHelper(expr)
        case EOBndExpr(_, Fix(expr)) => exprHelper(expr)
      }

//    code.bnds.flatMap(bndHelper).toList
  }

  def main(args: Array[String]): Unit = {
    val code: String = "a.b.c.d.e.f"

    println(Parser.parse(code))
  }

}
