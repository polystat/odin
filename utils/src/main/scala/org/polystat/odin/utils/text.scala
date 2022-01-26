package org.polystat.odin.utils

object text {
  def indentWith(c: Char)(n: Int)(s: String): String = s"${c.toString * n}$s"

  def indentSpace: Int => String => String = indentWith(' ')

  def indent: String => String = indentWith(' ')(2)

  // Here are the rules for escaping in json
  val decodeTable: Map[Char, Char] =
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


  private val encodeTable =
    decodeTable.iterator.map { case (v, k) => (k, s"\\$v") }.toMap

  private val nonPrintEscape: Array[String] =
    (0 until 32).map { c =>
      val strHex = c.toHexString
      val strPad = List.fill(4 - strHex.length)('0').mkString
      s"\\u$strPad$strHex"
    }.toArray



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
                    // \c is interpreted as just \c, if the character isn't
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
