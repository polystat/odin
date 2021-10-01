package org.polystat.odin.parser

import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

private[parser] object Utils {

  def createArrayFromNonEmpty(
    ne: Option[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]]
  ): EOExprOnly = Fix[EOExpr](
    EOArray(ne.map(_.value).getOrElse(Vector.empty[EOBnd[EOExprOnly]]))
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
    args: Vector[EOBnd[EOExprOnly]]
  ): EOExprOnly =
    Fix[EOExpr](
      NonEmpty
        .from(args.tail)
        .map { tail =>
          EOCopy(
            Fix[EOExpr](EODot(extractEOExpr(args.head), id)),
            tail
          )
        }
        .getOrElse(EODot(extractEOExpr(args.head), id))
    )

}
