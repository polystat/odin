package org.polystat.odin.parser

import cats.data.NonEmptyVector
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

private[parser] object Utils {

  def createArrayFromNonEmpty(
    ne: Option[NonEmptyVector[EOBnd[EOExprOnly]]]
  ): EOExprOnly = Fix[EOExpr](
    EOArray(ne.map(_.toVector).getOrElse(Vector.empty[EOBnd[EOExprOnly]]))
  )

  private def extractEOExpr(bnd: EOBnd[EOExprOnly]): EOExprOnly = {
    bnd match {
      case EOAnonExpr(expr) => expr
      case EOBndExpr(_, expr) => expr
    }
  }

  // TODO: rewrite so that the information
  //  about names of bindings is not lost
  def createInverseDot(
    id: String,
    args: NonEmptyVector[EOBnd[EOExprOnly]]
  ): EOExprOnly =
    Fix[EOExpr](
      NonEmptyVector
        .fromVector(args.tail)
        .map { tail =>
          EOCopy(
            Fix[EOExpr](EODot(extractEOExpr(args.head), id)),
            tail
          )
        }
        .getOrElse(EODot(extractEOExpr(args.head), id))
    )

}
