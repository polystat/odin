package org.polystat.odin.parser.cats_parse

import org.apache.commons.text.StringEscapeUtils
import cats.parse.{Parser => P, Parser0 => P0}

/**
  * borrowed from:
  * https://github.com/typelevel/cats-parse/blob/main/bench/src/main/scala/cats/parse/bench/self.scala
  */
object JsonStringUtil extends GenericStringUtil {

  // Here are the rules for escaping in json
  lazy val decodeTable: Map[Char, Char] =
    Map(
      ('\\', '\\'),
      ('\'', '\''),
      ('\"', '\"'),
      ('b', '\b'), // backspace
      ('f', '\f'), // form-feed
      ('n', '\n'),
      ('r', '\r'),
      ('t', '\t')
    )

}

abstract class GenericStringUtil {
  protected def decodeTable: Map[Char, Char]

  private val encodeTable =
    decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray

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
    (P.char('\\').as("\\") ~ after).map { case (bs, after) =>
      val unescaped = StringEscapeUtils.unescapeJava(bs + after)
      require(unescaped.length == 1)
      unescaped.charAt(0)
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

  def escape(quoteChar: Char, str: String): String = {
    // We can ignore escaping the opposite character used for the string
    // x isn't escaped anyway and is kind of a hack here
    val ignoreEscape =
      if (quoteChar == '\'') '"' else if (quoteChar == '"') '\'' else 'x'
    str.flatMap { c =>
      if (c == ignoreEscape) c.toString
      else
        encodeTable.get(c) match {
          case None =>
            if (c < ' ') nonPrintEscape(c.toInt)
            else c.toString
          case Some(esc) => esc
        }
    }
  }

  def unescape(str: String): Either[Int, String] = {
    val sb = new java.lang.StringBuilder
    def decodeNum(idx: Int, size: Int, base: Int): Int = {
      val end = idx + size
      if (end <= str.length) {
        val intStr = str.substring(idx, end)
        val asInt =
          try Integer.parseInt(intStr, base)
          catch { case _: NumberFormatException => ~idx }
        sb.append(asInt.toChar)
        end
      } else ~str.length
    }
    @annotation.tailrec
    def loop(idx: Int): Int =
      if (idx >= str.length) {
        // done
        idx
      } else if (idx < 0) {
        // error from decodeNum
        idx
      } else {
        val c0 = str.charAt(idx)
        if (c0 != '\\') {
          sb.append(c0)
          loop(idx + 1)
        } else {
          // str(idx) == \
          val nextIdx = idx + 1
          if (nextIdx >= str.length) {
            // error we expect there to be a character after \
            ~idx
          } else {
            val c = str.charAt(nextIdx)
            decodeTable.get(c) match {
              case Some(d) =>
                sb.append(d)
                loop(idx + 2)
              case None =>
                c match {
                  case 'o' => loop(decodeNum(idx + 2, 2, 8))
                  case 'x' => loop(decodeNum(idx + 2, 2, 16))
                  case 'u' => loop(decodeNum(idx + 2, 4, 16))
                  case 'U' => loop(decodeNum(idx + 2, 8, 16))
                  case other =>
                    // \c is interpretted as just \c, if the character isn't
                    // escaped
                    sb.append('\\')
                    sb.append(other)
                    loop(idx + 2)
                }
            }
          }
        }
      }

    val res = loop(0)
    if (res < 0) Left(~res)
    else Right(sb.toString)
  }

}
