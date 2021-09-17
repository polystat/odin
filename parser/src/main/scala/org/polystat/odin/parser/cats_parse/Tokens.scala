package org.polystat.odin.parser.cats_parse

import cats.parse.Parser.void
import cats.parse.Rfc5234.{crlf, lf, wsp}
import cats.parse.{Parser0, Parser => P}

object Tokens {

  lazy val singleLineWhitespace: P[Unit] = void(wsp.rep(1))
  lazy val eol: P[Unit] = (wsp.rep0.with1 ~ (crlf | lf)).void
  lazy val emptyLinesOrComments: Parser0[Unit] = (
    (wsp.rep0 *> (P.char('#') *> P.charsWhile0(_ != '\n')).?).with1 *>
      eol
    ).rep0.void

  lazy val digit: P[Char] = P.charIn('0' to '9')
  lazy val letter: P[Char] = P.ignoreCaseCharIn('a' to 'z')
  lazy val lowercase: P[Char] = P.charIn('a' to 'z')
  lazy val identifier: P[String] = (
    lowercase ~
      (letter | digit | P.charIn('-') | P.charIn('_')).rep0
    ).map {
    case (c, value) => (c :: value).mkString
  }


}
