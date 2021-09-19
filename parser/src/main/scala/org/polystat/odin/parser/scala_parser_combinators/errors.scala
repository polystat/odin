package org.polystat.odin.parser.scala_parser_combinators

object errors {
  sealed trait ParsingError {
    val msg: String
  }
  sealed case class LexerError(override val msg: String) extends ParsingError
  sealed case class ParserError(override val msg: String) extends ParsingError
}
