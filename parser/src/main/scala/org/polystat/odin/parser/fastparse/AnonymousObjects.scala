package org.polystat.odin.parser.fastparse

import fastparse.SingleLineWhitespace._
import fastparse._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.Utils.{
  createArrayFromNonEmpty,
  createInverseDot
}
import org.polystat.odin.parser.fastparse.Common._
import org.polystat.odin.parser.fastparse.SingleLineApplication.{
  args,
  singleLineApplication
}

private[parser] object AnonymousObjects {

  def anonymousObject[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] =
    anonymousAbstraction(indent, indentationStep) | anonymousApplication(
      indent,
      indentationStep
    )

  def anonymousApplication[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] = P(
    anonymousInverseDotApplication(
      indent,
      indentationStep
    ) | anonymousRegularApplication(indent, indentationStep)
  )

  def anonymousRegularApplication[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] = P(
    singleLineApplication ~ verticalApplicationArgs(indent, indentationStep).?
  ).map {
    case (trg, Some(args)) => EOAnonExpr(
        Fix[EOExpr](EOCopy(trg, args))
      )
    case (trg, None) => EOAnonExpr(trg)
  }

  def anonymousInverseDotApplication[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ verticalApplicationArgs(indent, indentationStep)
  ).map { case (id, args) =>
    EOAnonExpr(createInverseDot(id, args))
  }

  def namedVerticalArray[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] = P(
    "*" ~/ verticalApplicationArgs(indent, indentationStep).?
  ).map { args =>
    EOAnonExpr(createArrayFromNonEmpty(args))
  }

  def anonymousAbstraction[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOAnonExpr[EOExprOnly]] = P(
    args ~/ boundAttributes(indent, indentationStep).?
  ).map { case (params, vararg, attrs) =>
    EOAnonExpr(
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }

}
