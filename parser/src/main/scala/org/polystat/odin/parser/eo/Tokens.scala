package org.polystat.odin.parser.eo

import cats.parse.Rfc5234.{crlf, lf}
import cats.parse.{Parser => P, _}

object Tokens {

  val wsp: P[Unit] = Rfc5234.wsp.rep(1).void
  val optWsp: Parser0[Unit] = Rfc5234.wsp.rep0.void
  val eol: P[Unit] = (optWsp.with1 ~ (crlf | lf)).void

  val emptyLinesOrComments: Parser0[Unit] = (
    (optWsp *>
      (P.char('#') *> P.charsWhile0(_ != '\n')).?).with1.soft
      *> eol
  ).rep0.void

  val digit: P[Char] = P.charIn('0' to '9')
  val letter: P[Char] = P.ignoreCaseCharIn('a' to 'z')
  val lowercase: P[Char] = P.charIn('a' to 'z')

  val identifier: P[String] = (
    lowercase ~
      (letter | digit | P.charIn('-') | P.charIn('_')).rep0
  ).string

  val float: P[Float] = {
    val digits: P[String] = digit.rep.string
    val exp: P[Unit] = (P.charIn("eE") ~ P.charIn("+-").? ~ digits).void

    ((Numbers.signedIntString.soft ~ P.char('.')).soft ~ digits ~ exp.?).string
  }.map(_.toFloat)

  val integer: P[Int] = Numbers.signedIntString.map(_.toInt)
  val string: P[String] = StringUtils.escapedString('\"')

  val char: P[Char] =
    (
      StringUtils.escapedToken.backtrack |
        P.charWhere(c => c != '\n' && c != '\'')
    )
      .surroundedBy(P.char('\''))

}
