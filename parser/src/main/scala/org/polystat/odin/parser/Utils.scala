package org.polystat.odin.parser

import cats.data.NonEmptyVector
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

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

  // This wants a rewrite that keeps the information
  //  about names of bindings
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
