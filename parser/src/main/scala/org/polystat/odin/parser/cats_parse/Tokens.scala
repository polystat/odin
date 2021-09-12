package org.polystat.odin.parser.cats_parse

import cats.parse.Parser.void
import cats.parse.Rfc5234.{crlf, lf, wsp}
import cats.parse.{Parser0, Parser => P}

object Tokens {

  val singleLineWhitespace: P[Unit] = void(wsp.rep(1))
  val eol: P[Unit] = (wsp.rep0.with1 ~ (crlf | lf)).void
  val emptyLinesOrComments: Parser0[Unit] = (
    (wsp.rep0 *> (P.char('#') *> P.charsWhile0(_ != '\n')).?).with1 *>
      eol
    ).rep0.void

  val digit: P[Char] = P.charIn('0' to '9')
  val letter: P[Char] = P.ignoreCaseCharIn('a' to 'z')
  val lowercase: P[Char] = P.charIn('a' to 'z')
  val identifier: P[String] = (
    lowercase ~
      (letter | digit | P.charIn('-') | P.charIn('_')).rep
    ).map {
    case (c, value) => (c :: value).toList.mkString
  }


}
