package eo.core.parser

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class WorkflowTokenReader(val tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[Token] = new WorkflowTokenReader(tokens.tail)
}

case class ParserError(msg: String) extends CompilationError


object Parser extends Parsers {
  override type Elem = Token

  def apply(tokens: Seq[Token]): Either[ParserError, EOProg] = {
    val reader = new WorkflowTokenReader(tokens)
    program(reader) match {
      case Success(result, _) => Right(result)
      case Error(msg, _) => Left(ParserError(msg))
      case Failure(msg, _) => Left(ParserError(msg))
    }
  }

//  private def identifier: Parser[IDENTIFIER] = {
//    accept("identifier", { case id: IDENTIFIER => id })
//  }
//
//  private def literal: Parser[LITERAL] = {
//    accept("literal", { case lit: LITERAL => lit })
//  }

  private def single_line_comment: Parser[SINGLE_LINE_COMMENT] = {
    accept("single line comment", { case comment: SINGLE_LINE_COMMENT => comment })
  }

  private def meta: Parser[META] = {
    accept("meta", { case meta: META => meta })
  }


  def program: Parser[EOProg] = {
    opt(license) ~ opt(metas) ^^ {
      case license ~ metas => EOProg(license, metas, Vector())
    }
  }

  def metas: Parser[EOMetas] = {
    rep1(meta) ^^ {
      metas => {

        def processOtherMetas(metas: List[META]): List[EOMeta] = {
          metas.map {
            case META(name, text) if name == "+rt" =>
              val split = text.split(' ').filterNot(_.isEmpty)
              EORTMeta(split(0), split(1))
            case META(name, text) if name == "+alias" =>
              val split = text.split(' ').filterNot(_.isEmpty)
              EOAliasMeta(split(0), split(1))
            case META(_, _) => throw new Exception()
          }
        }

        metas match {
          case META(name, text) :: tail if name == "+package" => EOMetas(Some(text), processOtherMetas(tail).toVector)
          case _ => EOMetas(None, processOtherMetas(metas).toVector)
        }
      }
    }
  }

  def license: Parser[EOLicense] = {
    rep1(single_line_comment) ^^ {
      lines => EOLicense(lines.map(_.text).mkString)
    }
  }

  //  def objects = {
  //    rep1(
  //      rep(single_line_comment) ~ `object`
  //    )
  //  }
  //
  //  def `object` = {
  //    anonymous |
  //      (abstraction | application) |
  //      opt(tail) ~ rep(
  //        method ~ opt(htail) ~ opt(suffix) ~ opt(tail)
  //      )
  //  }
  //
  //  def anonymous = {
  //    attributes ~ htail
  //  }
  //
  //  def abstraction = {
  //    rep(single_line_comment) ~ attributes ~ opt(suffix ~ opt(SLASH ~ identifier))
  //  }
  //
  //  def attributes = {
  //    LBRACKET ~ rep(attribute) ~ RBRACKET
  //  }
  //
  //  def attribute = {
  //    label
  //  }
  //
  //  def label = {
  //    PHI | identifier ~ opt(DOTS)
  //  }
  //
  //  def tail = {
  //    INDENT ~ rep1(`object`) ~ DEDENT
  //  }
  //
  //  def suffix = {
  //    ASSIGN_NAME ~ label ~ opt(EXCLAMATION_MARK)
  //  }
  //
  //  def method = {
  //    DOT ~ (identifier | RHO | PHI)
  //  }
  //
  //  def application = {
  //    htail ~ opt(htail)
  //  }
  //
  //  def htail = {
  //    rep1(
  //      head
  //        | application ~ method
  //        | LPAREN ~ application ~ RPAREN
  //        | application ~ has
  //        | application ~ suffix
  //        | anonymous
  //    )
  //  }
  //
  //  def head = {
  //    PHI | RHO | SELF | ARRAY_DELIMITER | identifier | identifier ~ DOT | data
  //  }
  //
  //  def has = {
  //    COLON ~ identifier
  //  }
  //
  //  def data = {
  //    literal ^^ {
  //      case STRING(value) => EOStrData(value)
  //      case CHAR(value) => EOCharData(value.charAt(0))
  //      case INTEGER(value) => EOIntData(value.toInt)
  //      case FLOAT(value) => EOFloatData(value.toFloat)
  //    }
  //  }
  def main(args: Array[String]): Unit = {
    val code =
      """
        |# Some license here
        |# Some more license
        |+package sandbox
        |+alias  stdout    org.eolang.io.stdout
        |+alias  sprintf org.eolang.txt.sprintf
        |+rt  jvm            java8-adopt
      """.stripMargin
    println(code)
    println(Lexer(code))
    println(apply(Lexer(code).getOrElse(List())))
  }

}
