package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.instances.progToEO
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps

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

object Context {

  type Context = Map[String, BigInt]

  def resolveLocator(
    ctx: Context,
    app: EOSimpleApp[Fix[EOExpr]],
    currentDepth: BigInt
  ): EOSimpleAppWithLocator[Fix[EOExpr]] = {
    val name: String = app.name
    val depth: BigInt = ctx.getOrElse(name, 0)

    EOSimpleAppWithLocator(app.name, currentDepth - depth)
  }

  def rebuildContext(
    ctx: Context,
    currentDepth: BigInt,
    objs: Vector[EOBnd[Fix[EOExpr]]],
    freeAttrs: Option[Vector[LazyName]]
  ): Context = {
    val objCtx = objs
      .flatMap {
        case bndExpr: EOBndExpr[Fix[EOExpr]] => Some(bndExpr)
        case _ => None
      }
      .map(bnd => bnd.bndName.name.name -> currentDepth)
      .toMap
    val argCtx = freeAttrs match {
      case Some(value) => value.map(lName => lName.name -> currentDepth).toMap
      case None => Map.empty
    }

    ctx ++ objCtx ++ argCtx
  }

  def setLocators(code: EOProg[Fix[EOExpr]]): EOProg[Fix[EOExpr]] = {
    def exprHelper(ctx: Context, depth: BigInt)(
      expr: EOExpr[Fix[EOExpr]]
    ): EOExpr[Fix[EOExpr]] =
      expr match {
        case obj @ EOObj(freeAttrs, _, bndAttrs) =>
          val newDepth = depth + 1
          val newCtx = rebuildContext(ctx, newDepth, bndAttrs, Some(freeAttrs))

          obj.copy(bndAttrs = bndAttrs.map(bndExprHelper(newCtx, newDepth)))

        case app: EOSimpleApp[Fix[EOExpr]] => resolveLocator(ctx, app, depth)
        case EOCopy(Fix(trg), args) =>
          EOCopy(
            trg = Fix(exprHelper(ctx, depth)(trg)),
            args = args.map(bndHelper(ctx, depth))
          )
        case dot @ EODot(Fix(src), _) =>
          dot.copy(src = Fix(exprHelper(ctx, depth)(src)))
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
      rebuildContext(Map(), 0, code.bnds, None)
    code.copy(bnds = code.bnds.map(bndHelper(initialCtx, 0)))
  }

  def main(args: Array[String]): Unit = {
    val code: String =
      """
        |[] > outer
        |  [] > self
        |    256 > magic
        |    [] > dummy
        |      [outer] > cock
        |        outer > @
        |      outer.self > @
        |    self "yahoo" > @
        |  [self] > method
        |    self.magic > @
        |     
        |""".stripMargin

    Parser
      .parse(code)
      .map(setLocators) match {
      case Left(value) => println(value)
      case Right(value) => println(value.toEOPretty)
    }

  }

}
