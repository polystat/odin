package eo.parser.fastparse

import fastparse._
import NoWhitespace._
import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast.astparams.EOExprOnly
import eo.core.ast._
import higherkindness.droste.data.Fix


object Metas {
  private def packageMeta[_: P] = P(
    "+package" ~ " " ~ packageName.! ~ "\n"
  )

  private def aliasMeta[_: P] = P(
    "+alias" ~ " " ~ aliasName.! ~ " " ~ packageName.! ~ "\n"
  ).map {
    case (alias, src) => EOAliasMeta(alias, src)
  }

  private def rtMeta[_: P] = P(
    "+rt" ~ " " ~ aliasName.! ~ " " ~ artifactId.! ~ "\n"
  ).map {
    case (rtName, src) => EORTMeta(rtName, src)
  }

  // TODO: refine rule
  private def aliasName[_: P] = P(Tokens.identifier)

  // TODO: refine rule
  private def artifactName[_: P] = P(Tokens.identifier)

  // TODO: refine rule
  private def artifactVersion[_: P] =
    P(Tokens.digit.rep(1).rep(3, sep = "."))
      .map(_.mkString(sep = "."))

  // TODO: refine rule
  private def artifactId[_: P] = P(
    packageName ~/ ":" ~ artifactName ~/ ":" ~ artifactVersion
  ).map(_.productIterator.mkString(":"))

  // TODO: refine rule
  private def packageName[_: P] = P(
    Tokens.identifier.rep(sep = ".", min = 1)
  ).map(_.mkString(sep = "."))

  def metas[_: P]: P[EOMetas] = P(
    Tokens.emptyLinesOrComments ~ packageMeta.? ~
      (Tokens.emptyLinesOrComments ~ (rtMeta | aliasMeta))./.rep
  ).map {
    case (pkg, metas) => EOMetas(pkg, metas.toVector)
  }
}

object Tokens {

  def comment[_: P]: P[Unit] = P(
    whitespaceChars ~ "#" ~ CharsWhile(_ != '\n', 0) ~ "\n"
  )

  def identifier[_: P]: P[String] =
    P(lowercase ~ (letter | digit | "-").rep).!

  private def letter[_: P] = P(lowercase | uppercase)

  private def lowercase[_: P] = P(CharIn("a-z"))

  private def uppercase[_: P] = P(CharIn("A-Z"))

  def digit[_: P]: P[String] = P(CharIn("0-9")).!

  def integer[_: P]: P[EOExprOnly] = P("-".? ~ CharsWhileIn("0-9")).!
    .map(int => Fix[EOExpr](EOIntData(int.toInt)))

  def char[_: P]: P[EOExprOnly] = P("\'" ~ letter.! ~ "\'")
    .map(c => Fix[EOExpr](EOCharData(c.charAt(0))))

  def string[_: P]: P[EOExprOnly] = P("\"" ~ CharsWhile(_ != '\"').! ~ "\"")
    .map(s => Fix[EOExpr](EOStrData(s)))

  private def whitespaceChars[_: P] = P(
    CharsWhile(c => c == '\t' || c == ' ' || c == '\r', 0)
  )

  def emptyLine[_: P]: P[Unit] = P(whitespaceChars ~ "\n")

  def emptyLinesOrComments[_: P]: P[Unit] = P(
    (emptyLine | comment).rep
  )
}

class Objects(
               val indent: Int = 0,
               val indentationStep: Int = 2
             ) {

  private def createNonEmpty(objs: Seq[EOBnd[EOExprOnly]])
  : NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]] = {
    NonEmpty.from(objs) match {
      case Some(value) => value.toVector
      case None => throw new Exception("1 or more arguments expected, got 0.")
    }
  }

  //  private def extractEOExpr(bnd: EOBnd[EOExprOnly]): EOExprOnly = {
  //    bnd match {
  //      case EOAnonExpr(expr) => expr
  //      case EOBndExpr(_, expr) => expr
  //    }
  //  }

  //  private def createInverseDot(id: String,
  //                               args: Vector[EOBnd[EOExprOnly]]): EOExprOnly = {
  //    if (args.tail.nonEmpty) {
  //      Fix[EOExpr](
  //        EOCopy(
  //          Fix[EOExpr](EODot(extractEOExpr(args.head), id)),
  //          createNonEmpty(args.tail)
  //        )
  //      )
  //    } else {
  //      Fix[EOExpr](EODot(extractEOExpr(args.head), id))
  //    }
  //  }


  def singleLineWhitespace[_: P]: P[Unit] = CharsWhileIn(" \t", 1)

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
    singleLineWhitespace ~ ">" ~ singleLineWhitespace ~/
      (Tokens.identifier | "@").! ~ "!".!.?
  ).map {
    case ("@", None) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  def `object`[_: P]: P[EOBnd[EOExprOnly]] = P(namedObject | anonymousObject)

  def namedObject[_: P]: P[EOBndExpr[EOExprOnly]] = namedAbstraction

  def anonymousObject[_: P]: P[EOAnonExpr[EOExprOnly]] =
    anonymousAbstraction | anonymousApplication

  def anonymousApplication[_: P]: P[EOAnonExpr[EOExprOnly]] = ???

  def namedApplication[_: P]: P[EOBndExpr[EOExprOnly]] = ???


  def attributeName[_: P]: P[String] = (Tokens.identifier | "$" | "@" | "^").!

  def data[_: P]: P[EOExprOnly] = P(Tokens.integer | Tokens.string | Tokens.char)

  def simpleApplicatitonTarget[_: P]: P[EOExprOnly] = P(
    data | attributeName.map(name => Fix[EOExpr](EOSimpleApp(name)))
  )


  def attributeChain[_: P]: P[EOExprOnly] = P(
    simpleApplicatitonTarget ~ "." ~ attributeName.rep(1, sep = ".")
  ).map {
    case (start, attrs) =>
      attrs.foldLeft(start)((acc, id) => Fix[EOExpr](EODot(acc, id)))
  }

  def applicationTarget[_: P]: P[EOExprOnly] = {
    attributeChain | simpleApplicatitonTarget
  }

  def parenthesized[_: P]: P[EOExprOnly] = P("(" ~ singleLineApplication ~ ")")

  def horizontalApplicationArgs[_: P]
  : P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    (applicationTarget | parenthesized).rep(1, sep = singleLineWhitespace)
  ).map(args => createNonEmpty(args.map(EOAnonExpr(_))))

  def justApplication[_: P]: P[EOExprOnly] = P(
    (parenthesized | applicationTarget) ~ horizontalApplicationArgs
  ).map {
    case (trg, args) => Fix[EOExpr](EOCopy(trg, args))
  }


  def singleLineApplication[_: P]: P[EOExprOnly] = {
    P(justApplication | parenthesized | applicationTarget)
  }

  def verticalApplicationArgs[_: P]
  : P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    ("\n" ~ Tokens.emptyLinesOrComments ~ deeper)./.flatMap(
      i => new Objects(indent = i).`object`
        .rep(1, sep = ("\n" ~ Tokens.emptyLinesOrComments ~ (" " * i))./)
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

  def deeper[_: P]: P[Int] = P(
    (" " * (indent + indentationStep)).!
  ).map(_.length)

  def boundAttributes[_: P]: P[Vector[EOBndExpr[EOExprOnly]]] = P(
    ("\n" ~ Tokens.emptyLinesOrComments ~ deeper)./.flatMap(
      i => new Objects(indent = i).namedObject
        .rep(sep = ("\n" ~ Tokens.emptyLinesOrComments ~ (" " * i))./)
    )
  ).map(_.toVector)
}

class EOParser {


}


object EOParser {
  def main(args: Array[String]): Unit = {


  }

}