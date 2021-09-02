package eo.parser.fastparse

import fastparse._, NoWhitespace._

object Tokens {

  def comment[_: P]: P[Unit] = P(
    "#" ~ CharsWhile(_ != '\n', 0) ~ "\n"
  )

  def identifier[_: P]: P[String] =
    P(lowercase ~ (letter | digit | "-").rep).!

  private def letter[_: P] = P(lowercase | uppercase)

  private def lowercase[_: P] = P(CharIn("a-z"))

  private def uppercase[_: P] = P(CharIn("A-Z"))

  private def digit[_: P] = P(CharIn("0-9"))

  def string[_: P]: P[String] = P("\"" ~ CharsWhile(_ != '\"').! ~ "\"")

  def meta[_: P]: P[(String, String)] = P(
    "+" ~ identifier.! ~ " " ~ CharsWhile(_ != '\n').! ~ "\n"
  )
}

class EOParser {


}


object EOParser {
  def main(args: Array[String]): Unit = {


  }

}