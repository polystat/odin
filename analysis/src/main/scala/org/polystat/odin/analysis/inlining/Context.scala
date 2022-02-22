package org.polystat.odin.analysis.inlining

import cats.data.EitherNel
import cats.syntax.either._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.data.{NonEmptyList => Nel}

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
  ): EitherNel[String, EOExprOnly] = {
    val result = ctx
      .get(name)
      .map(depth => {
        val tmp: EOSimpleAppWithLocator[EOExprOnly] =
          EOSimpleAppWithLocator(name, currentDepth - depth)
        Fix(tmp)
      })

    Either.fromOption(
      result,
      Nel.one(s"Could not set locator for non-existent object with name $name")
    )
  }

  def rebuildContext(
    ctx: Context,
    currentDepth: BigInt,
    objs: Vector[EOBnd[EOExprOnly]],
    freeAttrs: Option[Vector[LazyName]]
  ): Context = {
    val objCtx = objs
      .collect { case bndExpr: EOBndExpr[Fix[EOExpr]] => bndExpr }
      .map(bnd => bnd.bndName.name.name -> currentDepth)
      .toMap
    val argCtx = freeAttrs match {
      case Some(value) => value.map(lName => lName.name -> currentDepth).toMap
      case None => Map.empty
    }

    ctx ++ objCtx ++ argCtx
  }

  def setLocators(
    code: EOProg[EOExprOnly]
  ): EitherNel[String, EOProg[EOExprOnly]] = {
    def recurse(ctx: Context, depth: BigInt)(
      expr: EOExprOnly
    ): EitherNel[String, EOExprOnly] =
      Fix.un(expr) match {
        case obj @ EOObj(freeAttrs, _, bndAttrs) =>
          val newDepth = depth + 1
          val newCtx = rebuildContext(ctx, newDepth, bndAttrs, Some(freeAttrs))

          Optics
            .traversals
            .eoObjBndAttrs
            .modifyA(recurse(newCtx, newDepth))(obj)
            .map(Fix(_))

        case app: EOSimpleApp[Fix[EOExpr]] =>
          resolveLocator(ctx, app.name, depth)

        case copy: EOCopy[EOExprOnly] =>
          Optics
            .traversals
            .eoCopy
            .modifyA(recurse(ctx, depth))(copy)
            .map(Fix(_))

        case dot: EODot[EOExprOnly] =>
          Optics
            .lenses
            .focusDotSrc
            .modifyA(recurse(ctx, depth))(dot)
            .map(Fix(_))
        case other => Right(Fix(other))
      }

    val initialCtx =
      rebuildContext(Map(), 0, code.bnds, None)
    val newBnds = code
      .bnds
      .traverse(bnd =>
        for {
          newExpr <- recurse(initialCtx, 0)(bnd.expr)
        } yield Optics.lenses.focusFromBndToExpr.replace(newExpr)(bnd)
      )

    newBnds.map(bnds => code.copy(bnds = bnds))

  }

}
