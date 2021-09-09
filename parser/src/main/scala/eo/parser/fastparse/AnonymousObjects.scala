package eo.parser.fastparse

import eo.core.ast.{EOAnonExpr, EOCopy, EOExpr, EOObj}
import eo.core.ast.astparams.EOExprOnly
import eo.parser.fastparse.SingleLineApplication.singleLineApplication
import eo.parser.fastparse.Utils.createInverseDot
import fastparse._, NoWhitespace._
import higherkindness.droste.data.Fix

class AnonymousObjects(
                        override val indent: Int = 0,
                        override val indentationStep: Int = 2
                      ) extends RespectsIndentation {


  def anonymousObject[_: P]: P[EOAnonExpr[EOExprOnly]] =
    anonymousAbstraction | anonymousApplication

  def anonymousApplication[_: P]: P[EOAnonExpr[EOExprOnly]] = P(
    anonymousInverseDotApplication | anonymousRegularApplication
  )

  def anonymousRegularApplication[_: P]: P[EOAnonExpr[EOExprOnly]] = P(
    singleLineApplication ~ verticalApplicationArgs.?
  ).map {
    case (trg, Some(args)) => EOAnonExpr(
      Fix[EOExpr](EOCopy(trg, args))
    )
    case (trg, None) => EOAnonExpr(trg)
  }

  def anonymousInverseDotApplication[_: P]: P[EOAnonExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ verticalApplicationArgs
  ).map {
    case (id, args) => EOAnonExpr(createInverseDot(id, args))
  }


  def anonymousAbstraction[_: P]: P[EOAnonExpr[EOExprOnly]] = P(
    args ~ boundAttributes.?
  ).map {
    case (params, vararg, attrs) => EOAnonExpr(
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }

}
