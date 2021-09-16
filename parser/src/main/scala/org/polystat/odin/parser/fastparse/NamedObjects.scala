package org.polystat.odin.parser.fastparse

import fastparse.SingleLineWhitespace._
import fastparse._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.Utils.{createArrayFromNonEmpty, createInverseDot}
import org.polystat.odin.parser.fastparse.SingleLineApplication.{args, singleLineApplication}

class NamedObjects(
                    override val indent: Int = 0,
                    override val indentationStep: Int = 2
                  ) extends RespectsIndentation {

  def name[_: P]: P[EONamedBnd] = P(
    ">" ~/ (Tokens.identifier | "@").! ~ "!".!.?
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
    singleLineApplication ~ name ~/ verticalApplicationArgs.?
  ).map {
    case (trg, name, Some(args)) => EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
    case (trg, name, None) => EOBndExpr(name, trg)
  }

  def namedInverseDotApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ name ~/ verticalApplicationArgs
  ).map {
    case (id, name, args) => EOBndExpr(name, createInverseDot(id, args))
  }

  def namedVerticalArray[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    "*" ~ name ~/ verticalApplicationArgs.?
  ).map {
    case (name, args) => EOBndExpr(
      bndName = name,
      expr = createArrayFromNonEmpty(args)
    )
  }


  def namedAbstraction[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    args ~ name ~/ boundAttributes.?
  ).map {
    case (params, vararg, name, attrs) => EOBndExpr(
      name,
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }


}
