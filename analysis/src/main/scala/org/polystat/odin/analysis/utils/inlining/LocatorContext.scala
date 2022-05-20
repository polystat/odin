package org.polystat.odin.analysis.utils.inlining

import cats.data.EitherNel
import cats.data.{NonEmptyList => Nel}
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.utils.Abstract.modifyExprWithState
import org.polystat.odin.analysis.utils.Optics._
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

object LocatorContext {

  type Context = Map[String, BigInt]

  val predefinedKeywords: Context = Map(
    "seq" -> 0,
    "assert" -> 0,
    "random" -> 0,
    "memory" -> 0,
    "cage" -> 0,
    "goto" -> 0,
    "heap" -> 0,
    "ram" -> 0,
    "try" -> 0,
  )

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
    val metaSymbols: Context = code
      .metas
      .metas
      .collect {
        case EOAliasMeta(Some(alias), _) => alias
        case EOAliasMeta(None, src) => src.last
      }
      .map((_, BigInt(0)))
      .toMap
    val initialCtx =
      rebuildContext(
        predefinedKeywords ++ metaSymbols,
        0,
        code.bnds,
        Vector()
      )
    val modify = modifyExprWithState(initialCtx)(modifyExpr =
      ctx =>
        depth =>
          expr =>
            Fix.un(expr) match {
              case EOSimpleApp(name) =>
                resolveLocator(ctx, name, depth)
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
