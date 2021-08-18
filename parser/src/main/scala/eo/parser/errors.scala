package eo.parser


object errors {
  sealed trait CompilationError
  sealed case class LexerError(msg: String) extends CompilationError
  sealed case class ParserError(msg: String) extends CompilationError
}
