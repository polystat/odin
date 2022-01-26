package org.polystat.odin.parser.eo

import cats.parse.{Parser => P, Parser0 => P0}
import org.polystat.odin.utils.text.{decodeTable, unescape}

/**
  * borrowed from:
  * https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/self.scala
  */

object StringUtils {

  val escapedToken: P[Char] = {
    val escapes = P.charIn(decodeTable.keys.toSeq).map(_.toString)

    val oct: P[Char] = P.charIn('0' to '7')
    val octP: P[String] = (P.char('o').as('o') ~ oct.rep(2, 2)).map {
      case (o, octs) => (o :: octs).toList.mkString
    }

    val hex: P[Char] = P.ignoreCaseCharIn(('0' to '9') ++ ('a' to 'f'))
    val hex2: P[String] = hex.rep(2, 2).map(_.toList.mkString)
    val hexP: P[String] = (P.char('x').as("x") ~ hex2).map { case (x, hex) =>
      x + hex
    }

    val hex4: P[String] = (hex2 ~ hex2).map { case (hex1, hex2) =>
      hex1 + hex2
    }
    val u4: P[String] = (P.char('u').as("u") ~ hex4).map { case (u, hex) =>
      u + hex
    }

    val after = P.oneOf(escapes :: octP :: hexP :: u4 :: Nil)
    (P.char('\\').as("\\") ~ after).flatMap { case (bs, after) =>
      unescape(bs + after).fold(
        _ => P.fail,
        char => {
          require(char.length == 1)
          P.pure(char.charAt(0))
        }
      )
    }
  }

  /**
    * String content without the delimiter
    */
  def undelimitedString(endP: P[Unit]): P[String] =
    escapedToken
      .backtrack
      .orElse((!endP).with1 ~ P.anyChar)
      .rep
      .string
      .flatMap { str =>
        unescape(str) match {
          case Right(str1) => P.pure(str1)
          case Left(_) => P.fail
        }
      }

  private val simpleString: P0[String] =
    P.charsWhile0(c => c >= ' ' && c != '"' && c != '\\')

  def escapedString(q: Char): P[String] = {
    val end: P[Unit] = P.char(q)
    end *> (simpleString <* end)
      .backtrack
      .orElse(undelimitedString(end) <* end)
  }


}
