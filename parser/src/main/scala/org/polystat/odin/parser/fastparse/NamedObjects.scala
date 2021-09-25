package org.polystat.odin.parser.fastparse

import fastparse.SingleLineWhitespace._
import fastparse._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.Utils.{createArrayFromNonEmpty, createInverseDot}
import org.polystat.odin.parser.fastparse.SingleLineApplication.{args, singleLineApplication}
import org.polystat.odin.parser.fastparse.Common._

private[parser] object NamedObjects {

  def name[_: P]: P[EONamedBnd] = P(
    ">" ~/ (Tokens.identifier | "@").! ~ "!".!.?
  ).map {
    case ("@", None) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  def namedObject[_: P](indent: Int, indentationStep: Int): P[EOBndExpr[EOExprOnly]] =
    namedAbstraction(indent, indentationStep) | namedApplication(indent, indentationStep)

  def namedApplication[_: P](indent: Int, indentationStep: Int): P[EOBndExpr[EOExprOnly]] = P(
    namedInverseDotApplication(indent, indentationStep) | namedRegularApplication(indent, indentationStep)
  )

  def namedRegularApplication[_: P](indent: Int, indentationStep: Int): P[EOBndExpr[EOExprOnly]] = P(
    singleLineApplication ~ name ~/ verticalApplicationArgs(indent, indentationStep).?
  ).map {
    case (trg, name, Some(args)) => EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
    case (trg, name, None) => EOBndExpr(name, trg)
  }

  def namedInverseDotApplication[_: P](indent: Int, indentationStep: Int): P[EOBndExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ name ~/ verticalApplicationArgs(indent, indentationStep)
  ).map {
    case (id, name, args) => EOBndExpr(name, createInverseDot(id, args))
  }

  def namedVerticalArray[_: P](indent: Int, indentationStep: Int): P[EOBndExpr[EOExprOnly]] = P(
    "*" ~ name ~/ verticalApplicationArgs(indent, indentationStep).?
  ).map {
    case (name, args) => EOBndExpr(
      bndName = name,
      expr = createArrayFromNonEmpty(args)
    )
  }

  def namedAbstraction[_: P](indent: Int, indentationStep: Int): P[EOBndExpr[EOExprOnly]] = P(
    args ~ name ~/ boundAttributes(indent, indentationStep).?
  ).map {
    case (params, vararg, name, attrs) => EOBndExpr(
      name,
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }
}
