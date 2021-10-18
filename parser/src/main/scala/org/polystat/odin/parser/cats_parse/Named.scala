package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.Utils._
import org.polystat.odin.parser.cats_parse.Common._
import org.polystat.odin.parser.cats_parse.SingleLine._
import org.polystat.odin.parser.cats_parse.Tokens._

object Named {

  val name: P[EONamedBnd] = (
    (optWsp.with1.soft *> (P.char('>') *> optWsp) *>
      (identifier | P.string("@").string)) ~
      (optWsp *> P.char('!').?) <* optWsp
  ).map {
    case ("@", None) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  type ApplicationArgs = NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]

  def `object`(
    indent: Int,
    indentationStep: Int
  ): P[EOBndExpr[EOExprOnly]] = {

    val abstraction =
      (
        params.soft ~ name ~
          boundAttributes(indent, indentationStep).?
      ).map { case (((params, vararg), name), attrs) =>
        EOBndExpr(
          name,
          Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
        )
      }

    val inverseDotApplication = (
      (identifier.soft <* P.char('.')).soft ~ name ~
        verticalApplicationArgs(indent, indentationStep)
    ).map { case ((attr, name), args) =>
      EOBndExpr(name, createInverseDot(attr, args))
    }

    val verticalArray = (
      (P.char('*').soft *> name).soft ~
        verticalApplicationArgs(indent, indentationStep)
    ).map { case (name, args) =>
      EOBndExpr(name, createArrayFromNonEmpty(Some(args)))
    }

    val regularApplication = (
      singleLineApplication.soft ~ name ~
        verticalApplicationArgs(indent, indentationStep).?
    ).map {
      case ((trg, name), Some(args)) =>
        EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
      case ((trg, name), None) => EOBndExpr(name, trg)
    }

    val application = inverseDotApplication | regularApplication

    P.defer(
      abstraction |
        verticalArray |
        application
    )
  }

}
