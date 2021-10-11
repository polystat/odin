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
    (P.char('>').surroundedBy(optWsp) *>
      (identifier | P.stringIn("@" :: Nil))) ~
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
        SingleLine.params ~ name ~
          boundAttributes(indent, indentationStep)
      ).map { case (((params, vararg), name), args) =>
        EOBndExpr(name, Fix[EOExpr](EOObj(params, vararg, args)))
      }

    val inverseDotApplication = (
      (identifier.soft <* P.char('.').surroundedBy(optWsp)) ~ name ~
        verticalApplicationArgs(indent, indentationStep)
    ).map { case ((attr, name), args) =>
      EOBndExpr(name, createInverseDot(attr, args))
    }

    val verticalArray = (
      (P.char('*') *> name) ~ verticalApplicationArgs(indent, indentationStep).?
    ).map { case (name, args) =>
      EOBndExpr(name, createArrayFromNonEmpty(args))
    }

    val regularApplication = (
      singleLineApplication.soft ~ name ~
        verticalApplicationArgs(indent, indentationStep).?
    ).map {
      case ((trg, name), Some(args)) =>
        EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
      case ((trg, name), None) => EOBndExpr(name, trg)
    }

    val application = inverseDotApplication.backtrack | regularApplication

    P.defer(
      abstraction |
        verticalArray |
        application
    )
  }

}
