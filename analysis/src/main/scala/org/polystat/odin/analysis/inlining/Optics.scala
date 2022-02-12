package org.polystat.odin.analysis.inlining

import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import monocle.{Lens, Optional, Prism}
import monocle.macros.GenLens
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import Inliner2.CopyArgs

object Optics {

  object prisms {

    val fixToEOObj: Prism[EOExprOnly, EOObj[EOExprOnly]] =
      Prism[EOExprOnly, EOObj[EOExprOnly]](fix =>
        Fix.un(fix) match {
          case obj: EOObj[EOExprOnly] => Some(obj)
          case _ => None
        }
      )(Fix(_))

    val fixToEOCopy: Prism[EOExprOnly, EOCopy[EOExprOnly]] =
      Prism[EOExprOnly, EOCopy[EOExprOnly]](fix =>
        Fix.un(fix) match {
          case copy: EOCopy[EOExprOnly] => Some(copy)
          case _ => None
        }
      )(Fix(_))

    val fixToEODot: Prism[EOExprOnly, EODot[EOExprOnly]] =
      Prism[EOExprOnly, EODot[EOExprOnly]](fix =>
        Fix.un(fix) match {
          case dot: EODot[EOExprOnly] => Some(dot)
          case _ => None
        }
      )(Fix(_))

    val fixToEOArray: Prism[EOExprOnly, EOArray[EOExprOnly]] =
      Prism[EOExprOnly, EOArray[EOExprOnly]](fix =>
        Fix.un(fix) match {
          case arr: EOArray[EOExprOnly] => Some(arr)
          case _ => None
        }
      )(Fix(_))

  }

  object lenses {

    val focusFromBndToExpr: Optional[EOBnd[EOExprOnly], EOExprOnly] =
      Lens[EOBnd[EOExprOnly], EOExprOnly](bnd => bnd.expr)(expr => {
        case bnd: EOBndExpr[EOExprOnly] => bnd.copy(expr = expr)
        case bnd: EOAnonExpr[EOExprOnly] => bnd.copy(expr = expr)
      })

    val focusFromBndExprToExpr: Lens[EOBndExpr[EOExprOnly], EOExprOnly] =
      GenLens[EOBndExpr[EOExprOnly]](_.expr)

    val focusFromEOObjToBndAttrs: Lens[EOObj[EOExprOnly], Vector[EOBndExpr[EOExprOnly]]] =
      GenLens[EOObj[EOExprOnly]](_.bndAttrs)

    val focusCopyTrg: Lens[EOCopy[EOExprOnly], EOExprOnly] =
      GenLens[EOCopy[EOExprOnly]](_.trg)

    val focusDotSrc: Lens[EODot[EOExprOnly], EOExprOnly] =
      GenLens[EODot[EOExprOnly]](_.src)

    val focusCopyArgs: Lens[EOCopy[EOExprOnly], CopyArgs] =
      GenLens[EOCopy[EOExprOnly]](_.args)

    val focusArrayElems: Lens[EOArray[EOExprOnly], Vector[EOBnd[EOExprOnly]]] =
      GenLens[EOArray[EOExprOnly]](_.elems)

  }

  def vectorIndexOptional[A](i: Int): Optional[Vector[A], A] =
    Optional[Vector[A], A](_.lift(i))(item =>
      seq => if (seq.isDefinedAt(i)) seq.updated(i, item) else seq
    )

  def nonEmptyVectorIndexOptional[A](
    i: Int
  ): Optional[NonEmpty[A, Vector[A]], A] =
    Optional[NonEmpty[A, Vector[A]], A](_.lift(i))(item =>
      seq => if (seq.isDefinedAt(i)) seq.updated(i, item) else seq
    )

  def focusBndAttrAtIndex(
    i: Int
  ): Optional[EOObj[EOExprOnly], EOExprOnly] =
    Optional[EOObj[EOExprOnly], EOExprOnly](obj =>
      obj.bndAttrs.lift(i).map(_.expr)
    )(expr =>
      obj =>
        lenses
          .focusFromEOObjToBndAttrs
          .andThen(vectorIndexOptional[EOBndExpr[EOExprOnly]](i))
          .andThen(lenses.focusFromBndExprToExpr)
          .replaceOption(expr)(obj)
          .getOrElse(obj)
    )

  def focusCopyArgAtIndex(
    i: Int
  ): Optional[EOCopy[EOExprOnly], EOExprOnly] =
    Optional[EOCopy[EOExprOnly], EOExprOnly](copy =>
      copy.args.lift(i).map(_.expr)
    )(expr =>
      copy => {
        lenses
          .focusCopyArgs
          .andThen(nonEmptyVectorIndexOptional[EOBnd[EOExprOnly]](i))
          .andThen(lenses.focusFromBndToExpr)
          .replaceOption(expr)(copy)
          .getOrElse(copy)
      }
    )

  def focusArrayElemAtIndex(i: Int): Optional[EOArray[EOExprOnly], EOExprOnly] =
    Optional[EOArray[EOExprOnly], EOExprOnly](arr =>
      arr.elems.lift(i).map(_.expr)
    )(expr =>
      arr =>
        lenses
          .focusArrayElems
          .andThen(vectorIndexOptional[EOBnd[EOExprOnly]](i))
          .andThen(lenses.focusFromBndToExpr)
          .replaceOption(expr)(arr)
          .getOrElse(arr)
    )

}
