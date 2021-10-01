package org.polystat.odin.parser.fastparse

import org.polystat.odin.core.ast.{EOCharData, EOExpr, EOIntData, EOStrData}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import fastparse._, NoWhitespace._
import higherkindness.droste.data.Fix

private[parser] object Tokens {

  def comment[_: P]: P[Unit] = P(
    whitespaceChars ~ "#" ~ CharsWhile(_ != '\n', 0) ~ "\n"
  )

  def identifier[_: P]: P[String] =
    P(lowercase ~ (letter | digit | "-" | "_").rep).!

  private def letter[_: P] = P(lowercase | uppercase)

  private def lowercase[_: P] = P(CharIn("a-z"))

  private def uppercase[_: P] = P(CharIn("A-Z"))

  def digit[_: P]: P[String] = P(CharIn("0-9")).!

  def integer[_: P]: P[EOExprOnly] = P("-".? ~ CharsWhileIn("0-9"))
    .!
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
