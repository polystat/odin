package org.polystat.odin.analysis.inlining

import cats.data.{EitherNel, NonEmptyList => Nel}
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Optics._
import Abstract.modifyExprWithState
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

// 0. Resolve explicit locator chains (^.^.aboba) during parsing
// 1. Create the first context that includes names of all top-lvl EOObjs
// 1.1. Replace All top-level applications (a > b) with application of
// 0-locators ($.a > b)
// 2. Recursively explore the AST
// 2.0 Upon meeting a non-existent identifier -> return Left with the
// corresponding error message
// 2.1 Upon meeting an EOSimpleAPP() -> resolve it using the current context
// into EOSimpleAppWithLocator()
// 2.2 Upon meeting a EOObj() -> use its contents to update the context and pass
// it in the next recursive call

object Context {

  type Context = Map[String, BigInt]

  def resolveLocator(
    ctx: Context,
    name: String,
    currentDepth: BigInt
  ): EitherNel[String, EOExprOnly] =
    ctx
      .get(name)
      .map(depth =>
        Fix[EOExpr](EOSimpleAppWithLocator(name, currentDepth - depth))
      )
      .toRight(
        Nel.one(
          s"Could not set locator for non-existent object with name \"$name\""
        )
      )

  def rebuildContext(
    ctx: Context,
    currentDepth: BigInt,
    objs: Vector[EOBnd[EOExprOnly]],
    freeAttrs: Vector[LazyName]
  ): Context = {
    val objCtx = objs
      .collect { case bndExpr: EOBndExpr[Fix[EOExpr]] => bndExpr }
      .map(bnd => (bnd.bndName.name.name, currentDepth))
      .toMap
    val argCtx = freeAttrs.map(lName => (lName.name, currentDepth)).toMap
    ctx ++ objCtx ++ argCtx
  }

  def setLocators(
    code: EOProg[EOExprOnly]
  ): EitherNel[String, EOProg[EOExprOnly]] = {
    val initialCtx =
      rebuildContext(
        Map(
          "seq" -> 0,
          "assert" -> 0,
        ),
        0,
        code.bnds,
        Vector()
      )
    val modify = modifyExprWithState(initialCtx)(modifyExpr =
      ctx =>
        depth =>
          expr =>
            Fix.un(expr) match {
              case app: EOSimpleApp[EOExprOnly] =>
                resolveLocator(ctx, app.name, depth)
              case other => Right(Fix(other))
            }
    )(modifyState =
      expr =>
        depth =>
          prev =>
            expr match {
              case EOObj(freeAttrs, _, bndAttrs) =>
                rebuildContext(prev, depth, bndAttrs, freeAttrs)
              case _ => prev
            }
    )(_)
    traversals.eoProg.modifyA(modify)(code)

  }

}
