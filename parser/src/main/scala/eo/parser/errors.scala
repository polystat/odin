package eo.parser


object errors {
  sealed trait ParsingError
  sealed case class LexerError(msg: String) extends ParsingError
  sealed case class ParserError(msg: String) extends ParsingError
}
