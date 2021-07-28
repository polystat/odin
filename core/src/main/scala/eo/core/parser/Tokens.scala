package eo.core.parser

sealed trait Token

// complex tokens
case class SINGLE_LINE_COMMENT(text: String) extends Token
case class IDENTIFIER(name: String) extends Token
case class INDENTATION(level: Int) extends Token
case class META(name: String, text: String) extends Token

sealed trait LITERAL extends Token {
  val value: String
}
case class STRING(value: String) extends LITERAL
case class CHAR(value: String) extends LITERAL
case class INTEGER(value: String) extends LITERAL
case class FLOAT(value: String) extends LITERAL

// delimiters
case object INDENT extends Token
case object DEDENT extends Token
case object LBRACKET extends Token
case object RBRACKET extends Token
case object LPAREN extends Token
case object RPAREN extends Token
case object ARRAY_DELIMITER extends Token

// standalone tokens
case object PHI extends Token
case object RHO extends Token
case object SELF extends Token
case object EXCLAMATION_MARK extends Token
case object COLON extends Token
case object DOT extends Token
case object PLUS extends Token
case object ASSIGN_NAME extends Token
case object SLASH extends Token
case object DOTS extends Token


