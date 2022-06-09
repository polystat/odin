package org.polystat.odin.analysis.utils

import cats.Applicative
import cats.Id
import cats.Monoid
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.semigroup._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import Optics.{lenses, traversals}

object Abstract {

  def modifyExprWithState[F[_]: Applicative, S](
    initialState: S,
    initialDepth: BigInt = 0
  )(modifyExpr: S => BigInt => EOExprOnly => F[EOExprOnly])(
    modifyState: EOExpr[EOExprOnly] => BigInt => S => S
  )(expr: EOExprOnly): F[EOExprOnly] = {
    def recurse(depth: BigInt)(state: S)(subExpr: EOExprOnly): F[EOExprOnly] = {
      Fix.un(subExpr) match {
        case obj: EOObj[EOExprOnly] =>
          traversals
            .eoObjBndAttrExprs
            .modifyA(recurse(depth + 1)(modifyState(obj)(depth + 1)(state)))(
              obj
            )
            .map(Fix(_))
        case copy: EOCopy[EOExprOnly] =>
          traversals
            .eoCopy
            .modifyA(recurse(depth)(modifyState(copy)(depth)(state)))(copy)
            .map(Fix(_))
        case dot: EODot[EOExprOnly] =>
          lenses
            .focusDotSrc
            .modifyA(recurse(depth)(modifyState(dot)(depth)(state)))(dot)
            .map(Fix(_))
        case array: EOArray[EOExprOnly] =>
          traversals
            .eoArrayElems
            .modifyA(recurse(depth)(modifyState(array)(depth)(state)))(array)
            .map(Fix(_))
        case other => modifyExpr(state)(depth)(Fix(other))
      }
    }
    recurse(initialDepth)(initialState)(expr)
  }

  def modifyExpr(
    modify: BigInt => EOExprOnly => EOExprOnly,
    initialDepth: BigInt = 0
  )(expr: EOExprOnly): EOExprOnly = {
    modifyExprWithState[Id, Unit]((), initialDepth)(_ =>
      depth => expr => modify(depth)(expr)
    )(_ => _ => identity)(expr)
  }

  def foldAst[A: Monoid](
    binds: Vector[EOBnd[EOExprOnly]],
    initialDepth: BigInt
  )(f: PartialFunction[(EOExpr[EOExprOnly], BigInt), A]): A = {
    def recurse(depth: BigInt)(bnd: EOBnd[EOExprOnly]): A = {
      f.lift((Fix.un(bnd.expr), depth)) match {
        case Some(value) => value
        case None => Fix.un(bnd.expr) match {
            case EOObj(_, _, bndAttrs) => bndAttrs.foldMap(recurse(depth + 1))
            case EOCopy(trg, args) =>
              recurse(depth)(EOAnonExpr(trg)).combine(
                args.foldMap(recurse(depth))
              )
            case EODot(trg, _) => recurse(depth)(EOAnonExpr(trg))
            case EOArray(elems) => elems.foldMap(recurse(depth))
            case _ => Monoid[A].empty
          }
      }
    }
    binds.foldMap(recurse(initialDepth))
  }

}
