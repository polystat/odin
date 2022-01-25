package org.polystat.odin.analysis.inlining

// import com.github.tarao.nonempty.collection.NonEmpty
// import eu.timepit.refined.api.Refined
// import eu.timepit.refined.auto._
// import eu.timepit.refined.numeric.NonNegative

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
// 0. Resolve explicit BigInt chains (^.^.aboba) during parsing
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

  //  type BigInt = BigInt // Refined[BigInt, NonNegative]
  type Context = Map[String, BigInt]

  def resolveLocator(
    ctx: Context,
    app: EOSimpleApp[Fix[EOExpr]]
  ): EOSimpleAppWithLocator[Fix[EOExpr]] = {
    val name: String = app.name
    val depth: BigInt = ctx.getOrElse(name, BigInt(0))

    EOSimpleAppWithLocator(app.name, depth)
  }

  def rebuildContext(
    ctx: Context,
    newDepth: BigInt,
    objs: Vector[EOBnd[Fix[EOExpr]]]
  ): Context = {
    val currentCtx = objs
      .flatMap {
        case bndExpr: EOBndExpr[Fix[EOExpr]] => Some(bndExpr)
        case _ => None
      }
      .map(bnd => bnd.bndName.name.name -> newDepth)
      .toMap

    ctx ++ currentCtx
  }

  def setLocators(code: EOProg[Fix[EOExpr]]): EOProg[Fix[EOExpr]] = {
    def exprHelper(ctx: Context, depth: BigInt)(
      expr: EOExpr[Fix[EOExpr]]
    ): EOExpr[Fix[EOExpr]] =
      expr match {
        case obj @ EOObj(_, _, bndAttrs) =>
          val newDepth = depth + BigInt(1)
          val newCtx = rebuildContext(ctx, newDepth, bndAttrs)

          obj.copy(bndAttrs = bndAttrs.map(bndExprHelper(newCtx, newDepth)))

        case app: EOSimpleApp[Fix[EOExpr]] => resolveLocator(ctx, app)
        case copy @ EOCopy(_, args) =>
          copy.copy(args = args.map(bndHelper(ctx, depth)))

        case other => other
      }

    def bndExprHelper(ctx: Context, depth: BigInt)(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): EOBndExpr[Fix[EOExpr]] =
      bnd match {
        case bnd @ EOBndExpr(_, Fix(expr)) =>
          bnd.copy(expr = Fix(exprHelper(ctx, depth)(expr)))
      }

    def bndHelper(ctx: Context, depth: BigInt)(
      bnd: EOBnd[Fix[EOExpr]]
    ): EOBnd[Fix[EOExpr]] =
      bnd match {
        case EOAnonExpr(Fix(expr)) =>
          EOAnonExpr(Fix(exprHelper(ctx, depth)(expr)))
        case bnd @ EOBndExpr(_, Fix(expr)) =>
          bnd.copy(expr = Fix(exprHelper(ctx, depth)(expr)))
      }

    val initialCtx =
      rebuildContext(Map(), BigInt(0), code.bnds)
    code.copy(bnds = code.bnds.map(bndHelper(initialCtx, BigInt(0))))
  }

  def main(args: Array[String]): Unit = {
    val code: String =
      """
        |[] > a
        |  [self] > g
        |    self.f self > @
        |  [self] > h
        |    self.i self > @
        |
        |
        |[] > a
        |  [self] > s
        |    2 > @
        |  [self] > f
        |    [] > aboba
        |      s > @
        |    self.h self > @
        |  [self] > i
        |    self.g self > @
        |""".stripMargin

    println(Parser.parse(code).map(setLocators))
  }

}
