package org.polystat.odin.parser.scala_parser_combinators

object errors {
  sealed trait ParsingError

  sealed case class LexerError(msg: String) extends ParsingError

  sealed case class ParserError(msg: String) extends ParsingError
}
