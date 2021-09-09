package eo.parser.fastparse

import eo.core.ast.{ConstName, EOAnyNameBnd, EOBndExpr, EOCopy, EODecoration, EOExpr, EONamedBnd, EOObj, LazyName}
import eo.core.ast.astparams.EOExprOnly
import eo.parser.fastparse.SingleLineApplication.singleLineApplication
import eo.parser.fastparse.Utils.createInverseDot
import fastparse._, SingleLineWhitespace._
import higherkindness.droste.data.Fix

class NamedObjects(
                    override val indent: Int = 0,
                    override val indentationStep: Int = 2
                  ) extends RespectsIndentation {

  def name[_: P]: P[EONamedBnd] = P(Pass ~ ">" ~
      (Tokens.identifier | "@").! ~ "!".!.?
  ).map {
    case ("@", None) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  def namedObject[_: P]: P[EOBndExpr[EOExprOnly]] =
    namedAbstraction | namedApplication

  def namedApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    namedInverseDotApplication | namedRegularApplication
  )

  def namedRegularApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    singleLineApplication ~ name ~ verticalApplicationArgs.?
  ).map {
    case (trg, name, Some(args)) => EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
    case (trg, name, None) => EOBndExpr(name, trg)
  }

  def namedInverseDotApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ name ~ verticalApplicationArgs
  ).map {
    case (id, name, args) => EOBndExpr(name, createInverseDot(id, args))
  }


  def namedAbstraction[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    args ~ name ~ boundAttributes.?
  ).map {
    case (params, vararg, name, attrs) => EOBndExpr(
      name,
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }


}
