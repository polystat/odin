package eo.core.parser

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

trait CompilationError
case class LexerError(msg: String) extends CompilationError


object Lexer extends RegexParsers {
  override def skipWhitespace = true

  override val whiteSpace: Regex = "[ \t\r\f]+".r

  def identifier: Parser[IDENTIFIER] = {
    "[a-z][a-z0-9_A-Z\\-]*".r ^^ {
      str => IDENTIFIER(str)
    }
  }

  def indentation: Parser[INDENTATION] = {
    "\n[ ]*".r ^^ { whitespace =>
      val nSpaces = whitespace.length - 1
      INDENTATION(nSpaces)
    }
  }

  def string: Parser[STRING] = {
    """"[^"]*"""".r ^^ {
      str => STRING(str)
    }
  }

  def integer: Parser[INTEGER] = {
    """[+-]?\b[0-9]+\b""".r ^^ {
      str => INTEGER(str)
    }
  }

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(
          lbracket |
          rbracket |
          lparen |
          rparen |
          single_line_comment |
          array_delimiter |

          phi |
          rho |
          self |
          exclamation_mark |
          colon |
          dot |
          plus |
          assign_name |

          identifier |
          indentation |
          string |
          integer
      )
    ) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case NoSuccess(msg, _) => Left(LexerError(msg))
      case Success(result, _) => Right(result)
      case Error(msg, _) => Left(LexerError(msg))
      case Failure(msg, _) => Left(LexerError(msg))
    }
  }

  private def processIndentations(tokens: List[Token],
                                  indents: List[Int] = List(0)): List[Token] = {
    tokens.headOption match {

      // if there is an increase in indentation level, we push this new level into the stack
      // and produce an INDENT
      case Some(INDENTATION(spaces)) if spaces > indents.head =>
        INDENT :: processIndentations(tokens.tail, spaces :: indents)

      // if there is a decrease, we pop from the stack until we have matched the new level,
      // producing a DEDENT for each pop
      case Some(INDENTATION(spaces)) if spaces < indents.head =>
        val (dropped, kept) = indents.partition(_ > spaces)
        (dropped map (_ => DEDENT)) ::: processIndentations(tokens.tail, kept)

      // if the indentation level stays unchanged, no tokens are produced
      case Some(INDENTATION(spaces)) if spaces == indents.head =>
        processIndentations(tokens.tail, indents)

      // other tokens are ignored
      case Some(token) =>
        token :: processIndentations(tokens.tail, indents)

      // the final step is to produce a DEDENT for each indentation level still remaining, thus
      // "closing" the remaining open INDENTS
      case None =>
        indents.filter(_ > 0).map(_ => DEDENT)

    }
  }


  def phi = "@" ^^ (_ => PHI)
  def rho = "^" ^^ (_ => RHO)
  def self = "$" ^^ (_ => SELF)
  def exclamation_mark = "!" ^^ (_ => EXCLAMATION_MARK)
  def colon = ":" ^^ (_ => COLON)
  def dot = "." ^^ (_ => DOT)
  def plus = "+" ^^ (_ => PLUS)
  def assign_name = ">" ^^ (_ => ASSIGN_NAME)
  def lbracket = "[" ^^ (_ => LBRACKET)
  def rbracket = "]" ^^ (_ => RBRACKET)
  def lparen = "(" ^^ (_ => LPAREN)
  def rparen = ")" ^^ (_ => RPAREN)
  def single_line_comment = "#" ^^ (_ => SINGLE_LINE_COMMENT)
  def array_delimiter = "*" ^^ (_ => ARRAY_DELIMITER)

}


object LexerMain extends App {
  val code =
    """+package sandbox
      |+alias stdout org.eolang.io.stdout
      |+alias sprintf org.eolang.txt.sprintf
      |
      |[] > base
      |  memory > x
      |  [v] > n
      |    seq > @
      |      stdout
      |        sprintf "Calling base.n with v = %d\n" v
      |      x.write v
      |  [v] > m
      |    seq > @
      |      stdout
      |        sprintf "Calling base.m with v = %d\n" v
      |      n v
      |
      |[] > derived
      |  base > @
      |  [v] > n
      |    seq > @
      |      stdout (sprintf "Calling derived.n with v = %d\n" v)
      |      ^.@.m v
      |
      |[args...] > app
      |  base > b
      |  derived > d
      |  seq > @
      |    b.n 10
      |    stdout
      |      sprintf
      |        "base:\n\tx after n = %d\n"
      |        b.x
      |    b.m 12
      |    stdout
      |      sprintf
      |        "\tx after m = %d\n"
      |        b.x
      |    d.n 5
      |    stdout
      |      sprintf
      |        "\nderived:\n\tx after n = %d\n"
      |        d.x
      |
      """.stripMargin
  println(code)
  println(Lexer(code))
}