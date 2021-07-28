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

  def single_line_comment: Parser[SINGLE_LINE_COMMENT] = {
      """#.*""".r ^^ {
      str => SINGLE_LINE_COMMENT(str.tail)
    }
  }

  def meta: Parser[META] = {
    """\+([a-z][a-z0-9_A-Z\-]*)[ ](.*)""".r ^^ {
      str => {
        val split = str.split(" ", 2)
        META(split(0), split(1))
      }
    }
  }

  def tokens: Parser[List[Token]] = {
    phrase(
      rep1(
          lbracket
            | rbracket
            | lparen
            | rparen
            | array_delimiter

            | phi
            | rho
            | self
            | exclamation_mark
            | colon
            | dot
            | assign_name

            | meta
            | identifier
            | indentation
            | string
            | integer
            | single_line_comment
      )
    ) ^^ { rawTokens =>
      processIndentations(rawTokens)
    }
  }

  def apply(code: String): Either[LexerError, List[Token]] = {
    parse(tokens, code) match {
      case Success(result, _) => Right(result)
      case Error(msg, _) => Left(LexerError("ERROR: " + msg))
      case Failure(msg, _) => Left(LexerError("FAILURE: " + msg))
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
  def assign_name = ">" ^^ (_ => ASSIGN_NAME)
  def lbracket = "[" ^^ (_ => LBRACKET)
  def rbracket = "]" ^^ (_ => RBRACKET)
  def lparen = "(" ^^ (_ => LPAREN)
  def rparen = ")" ^^ (_ => RPAREN)
  def array_delimiter = "*" ^^ (_ => ARRAY_DELIMITER)
  def slash = "/" ^^ (_ => SLASH)
  def dots = "..." ^^ (_ => DOTS)

}


object LexerMain extends App {
  val code =
    """
      |# Some license here
      |+package sandbox
      |+alias stdout org.eolang.io.stdout
      |+alias sprintf org.eolang.txt.sprintf
      |[] > base
      |  memory > x
      |  [self v] > f
      |    x.write > @
      |      v
      |  [self v] > g
      |    self.f > @
      |      self
      |      v
      |[] > derived
      |  base > @
      |  [self v] > f
      |    self.g > @
      |      self
      |      v
      """.stripMargin
  println(code)
  println(Lexer(code))
}