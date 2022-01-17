package org.polystat.odin.parser.eo

import cats.parse.{Parser => P}
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Tokens._
import org.polystat.odin.parser.Utils._
import org.polystat.odin.parser.eo.Common._
import org.polystat.odin.parser.eo.SingleLine._

object Anon {

  def `object`(
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] = {

    val regularApplication: P[EOAnonExpr[EOExprOnly]] = (
      singleLineApplication ~
        verticalApplicationArgs(indent, indentationStep).?
    ).map {
      case (trg, Some(args)) => EOAnonExpr(
          Fix[EOExpr](EOCopy(trg, args))
        )
      case (trg, None) => EOAnonExpr(trg)
    }

    val inverseDotApplication: P[EOAnonExpr[EOExprOnly]] = (
      (identifier.soft <* P.char('.')).soft ~
        verticalApplicationArgs(indent, indentationStep)
    ).map { case (id, args) =>
      EOAnonExpr(createInverseDot(id, args))
    }

    val application: P[EOAnonExpr[EOExprOnly]] =
      inverseDotApplication | regularApplication

    val verticalArray: P[EOAnonExpr[EOExprOnly]] = (
      P.char('*').soft *>
        verticalApplicationArgs(indent, indentationStep)
    ).map { args =>
      EOAnonExpr(createArrayFromNonEmpty(Some(args)))
    }

    val abstraction: P[EOAnonExpr[EOExprOnly]] = (
      params ~ boundAttributes(indent, indentationStep).?
    ).map { case ((params, vararg), attrs) =>
      EOAnonExpr(
        Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
      )
    }

    P.defer(
      application |
        abstraction |
        verticalArray
    )
  }

}
