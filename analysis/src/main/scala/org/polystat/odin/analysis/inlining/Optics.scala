package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import monocle.{Lens, Optional, Prism}
import monocle.macros.GenLens
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

object Optics {

  def seqIndexOptional[A](i: Int): Optional[Vector[A], A] =
    Optional[Vector[A], A](_.lift(i))(item =>
      seq => if (seq.isDefinedAt(i)) seq.updated(i, item) else seq
    )

  val focusFromBndExprToExpr: Lens[EOBndExpr[EOExprOnly], EOExprOnly] =
    GenLens[EOBndExpr[EOExprOnly]](_.expr)

  val focusFromEOObjToBndAttrs: Lens[EOObj[EOExprOnly], Vector[EOBndExpr[EOExprOnly]]] =
    GenLens[EOObj[EOExprOnly]](_.bndAttrs)

  val prismFromFixToEOObj: Prism[EOExprOnly, EOObj[EOExprOnly]] =
    Prism[EOExprOnly, EOObj[EOExprOnly]] {
      case obj: EOObj[EOExprOnly] => Some(obj)
      case _ => None
    }(Fix(_))

  def focusFromObjToItsBndAttrAtIndex(
    i: Int
  ): Optional[EOObj[EOExprOnly], EOExprOnly] =
    Optional[EOObj[EOExprOnly], EOExprOnly](obj =>
      obj.bndAttrs.lift(i).map(_.expr)
    )(expr =>
      obj =>
        focusFromEOObjToBndAttrs
          .andThen(seqIndexOptional[EOBndExpr[EOExprOnly]](i))
          .andThen(focusFromBndExprToExpr)
          .replaceOption(expr)(obj)
          .getOrElse(obj)
    )

}
