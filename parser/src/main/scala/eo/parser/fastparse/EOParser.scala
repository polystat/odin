package eo.parser.fastparse

import fastparse._
import NoWhitespace._
import eo.core.ast.{EOAliasMeta, EOMetas, EORTMeta}


object Metas {
  private def packageMeta[_: P] = P(
    "+package" ~ " " ~ packageName.! ~ "\n"
  )

  private def aliasMeta[_:P] = P(
    "+alias" ~ " " ~ aliasName.! ~ " " ~ packageName.! ~ "\n"
  ).map {
    case (alias, src) => EOAliasMeta(alias, src)
  }

  private def rtMeta[_:P] = P(
    "+rt" ~ " " ~ aliasName.! ~ " " ~ artifactId.! ~ "\n"
  ).map {
    case (rtName, src) => EORTMeta(rtName, src)
  }

  // TODO: refine rule
  private def aliasName[_ : P] = P(
    Tokens.identifier
  )

  // TODO: refine rule
  private def artifactName[_:P] =
    P(Tokens.identifier)

  // TODO: refine rule
  private def artifactVersion[_:P] =
    P(Tokens.digit.rep(1).rep(3, sep = "."))
      .map(_.mkString(sep = "."))

  // TODO: refine rule
  private def artifactId[_:P] = P(
    packageName ~/ ":" ~ artifactName ~/ ":" ~ artifactVersion
  ).map(_.productIterator.mkString(":"))

  // TODO: refine rule
  private def packageName[_: P] = P(
    Tokens.identifier.rep(sep = ".", min = 1)
  ).map(_.mkString(sep="."))

  def metas[_: P]: P[EOMetas] = P(
    Tokens.emptyLinesOrComments ~ packageMeta.? ~
      (Tokens.emptyLinesOrComments ~ (rtMeta | aliasMeta))./.rep
  ).map {
    case (pkg, metas) => EOMetas(pkg, metas.toVector)
  }
}

object Tokens {

  def comment[_: P]: P[Unit] = P(
    "#" ~ CharsWhile(_ != '\n', 0) ~ "\n"
  )

  def identifier[_: P]: P[String] =
    P(lowercase ~ (letter | digit | "-").rep).!

  private def letter[_: P] = P(lowercase | uppercase)

  private def lowercase[_: P] = P(CharIn("a-z"))

  private def uppercase[_: P] = P(CharIn("A-Z"))

  def digit[_: P]: P[String] = P(CharIn("0-9")).!

  def string[_: P]: P[String] = P("\"" ~ CharsWhile(_ != '\"').! ~ "\"")

  private def whitespaceChars[_: P] = P(
    CharsWhile(c => c == '\t' || c == ' ' || c == '\r', 0)
  )

  def emptyLine[_: P]: P[Unit] = P(
    whitespaceChars ~ "\n"
  )

  def emptyLinesOrComments[_ : P]: P[Unit] = P(
    (emptyLine | comment)./.rep
  )
}

class EOParser {


}


object EOParser {
  def main(args: Array[String]): Unit = {


  }

}