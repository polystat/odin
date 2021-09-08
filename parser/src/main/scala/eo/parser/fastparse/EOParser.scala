package eo.parser.fastparse

import fastparse._
import NoWhitespace._
import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast.astparams.EOExprOnly
import eo.core.ast._
import eo.parser.fastparse.SingleLineApplication.singleLineApplication
import eo.parser.fastparse.Tokens._
import eo.parser.fastparse.Utils._
import higherkindness.droste.data.Fix


class Objects(val indent: Int = 0, val indentationStep: Int = 2) {

  def deeper[_: P]: P[Int] = P(
    (" " * (indent + indentationStep)).!
  ).map(_.length)

  def args[_: P]: P[(Vector[LazyName], Option[LazyName])] = P(
    "[" ~
      (Tokens.identifier | "@").!.rep(sep = singleLineWhitespace) ~ "...".!.? ~
      "]"
  ).map {
    case (args, None) =>
      (args.map(LazyName).toVector, None)
    case (args, Some(_)) =>
      (args.map(LazyName).toVector.init, Some(LazyName(args.last)))
  }

  def name[_: P]: P[EONamedBnd] = P(
    singleLineWhitespace ~ ">" ~ singleLineWhitespace ~
      (Tokens.identifier | "@").! ~ "!".!.?
  ).map {
    case ("@", None) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  def program[_: P]: P[EOProg[EOExprOnly]] = P(
    Start ~
      Metas.metas ~
      Tokens.emptyLinesOrComments ~
      `object`.rep(sep = Tokens.emptyLinesOrComments) ~
      Tokens.emptyLinesOrComments ~
      End
  ).map {
    case (metas, objs) => EOProg(
      metas = metas,
      bnds = objs.toVector
    )
  }

  def `object`[_: P]: P[EOBnd[EOExprOnly]] = P(
    namedObject | anonymousObject
  )

  def namedObject[_: P]: P[EOBndExpr[EOExprOnly]] =
    namedAbstraction | namedApplication

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

  def namedApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    namedInverseDotApplication | namedRegularApplication
  )

  def namedRegularApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    singleLineApplication ~ name ~ verticalApplicationArgs.?
  ).map {
    case (trg, name, Some(args)) => EOBndExpr(name, Fix[EOExpr](EOCopy(trg, args)))
    case (trg, name, None) => EOBndExpr(name, trg)
  }

  def anonymousInverseDotApplication[_: P]: P[EOAnonExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ verticalApplicationArgs
  ).map {
    case (id, args) => EOAnonExpr(createInverseDot(id, args))
  }

  def namedInverseDotApplication[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    Tokens.identifier ~ "." ~ name ~ verticalApplicationArgs
  ).map {
    case (id, name, args) => EOBndExpr(name, createInverseDot(id, args))
  }

  def verticalApplicationArgs[_: P]
  : P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    ("\n" ~ Tokens.emptyLinesOrComments ~ deeper).flatMap(
      i => new Objects(indent = i).`object`
        .rep(1, sep = "\n" ~ Tokens.emptyLinesOrComments ~ (" " * i))
    )
  ).map(createNonEmpty)

  def anonymousAbstraction[_: P]: P[EOAnonExpr[EOExprOnly]] = P(
    args ~ boundAttributes.?
  ).map {
    case (params, vararg, attrs) => EOAnonExpr(
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }

  def namedAbstraction[_: P]: P[EOBndExpr[EOExprOnly]] = P(
    args ~ name ~ boundAttributes.?
  ).map {
    case (params, vararg, name, attrs) => EOBndExpr(
      name,
      Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
    )
  }

  def boundAttributes[_: P]: P[Vector[EOBndExpr[EOExprOnly]]] = P(
    ("\n" ~ Tokens.emptyLinesOrComments ~ deeper).flatMap(
      i => new Objects(indent = i).namedObject
        .rep(sep = "\n" ~ Tokens.emptyLinesOrComments ~ (" " * i))
    )
  ).map(_.toVector)
}

class EOParser {


}


object EOParser {
  def main(args: Array[String]): Unit = {


  }

}