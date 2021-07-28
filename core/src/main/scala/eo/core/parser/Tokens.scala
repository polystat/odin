package eo.core.parser

sealed trait Token

// complex tokens
case class COMMENT(text: String) extends Token
case class IDENTIFIER(name: String) extends Token
case class STRING(value: String) extends Token
case class CHAR(value: String) extends Token
case class INTEGER(value: String) extends Token
case class FLOAT(value: String) extends Token
case class INDENTATION(level: Int) extends Token

// delimiters
case object INDENT extends Token
case object DEDENT extends Token
case object LBRACKET extends Token
case object RBRACKET extends Token
case object LPAREN extends Token
case object RPAREN extends Token
case object SINGLE_LINE_COMMENT extends Token
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


