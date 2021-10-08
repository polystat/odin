package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.cats_parse.Tokens._
import org.polystat.odin.parser.Utils._
import org.polystat.odin.parser.cats_parse.Common._
import org.polystat.odin.parser.cats_parse.SingleLine._

object Named {

  val name: P[EONamedBnd] = (
    (P.char('>').surroundedBy(optWsp) *>
      (identifier | P.stringIn("@" :: Nil))) ~
      P.char('!').surroundedBy(optWsp).?
  ).map {
    case ("@", None) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  def named[L, R](
    left: P[L],
    right: P[R],
    createBnd: (L, R) => EOExprOnly
  ): P[EOBndExpr[EOExprOnly]] = {
    (left ~ name ~ right).map { case ((left, name), right) =>
      EOBndExpr(name, createBnd(left, right))
    }
  }

  def `object`(
    indent: Int,
    indentationStep: Int
  ): P[EOBndExpr[EOExprOnly]] = {

    val abstraction = named(
      SingleLine.params,
      boundAttributes(indent, indentationStep),
      { case ((params, vararg), bndAttrs) =>
        Fix[EOExpr](EOObj(params, vararg, bndAttrs))
      }
    )

    val inverseDotApplication = named(
      identifier <* P.char('.').surroundedBy(optWsp),
      verticalApplicationArgs(indent, indentationStep),
      { case (id, args) =>
        createInverseDot(id, args)
      }
    )

    val verticalArray = named(
      P.char('*'),
      verticalApplicationArgs(indent, indentationStep),
      { case (_, args) =>
        createArrayFromNonEmpty(args)
      }
    )

    val regularApplication = (
      singleLineApplication ~ name ~
        verticalApplicationArgs(indent, indentationStep).?
    ).map {
      case ((trg, name), Some(args)) =>
        EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
      case ((trg, name), None) => EOBndExpr(name, trg)
    }

    val application = regularApplication | inverseDotApplication

    P.defer(
      abstraction |
        application |
        verticalArray
    )
  }

}
