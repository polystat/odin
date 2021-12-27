package org.polystat.odin.parser.cats_parse

// Borrowed from:
// https://github.com/typelevel/cats-parse/issues/239
// In particular, from:
// https://gist.github.com/re-xyr/b0f1988e744c93d4c83a5f17f58484ea

import cats.data.NonEmptyList
import cats.parse.Parser.Expectation
import cats.parse._
import org.polystat.odin.parser.cats_parse.JsonStringUtil.escape

class Prettyprint(filename: String = "", input: String) {
  private val locmap = LocationMap(input)

  def description(x: Parser.Expectation): String = x match {
    case Expectation.OneOfStr(_, strs) =>
      val strList = strs.map { x => s"'${escape('\'', x)}'" }.mkString(", ")
      s"one of $strList"
    case Expectation.InRange(_, lower, upper) =>
      if (lower == upper) s"'${escape('\'', lower.toString)}'"
      else s"'$lower' ~ '$upper'"
    case Expectation.StartOfString(_) =>
      "beginning of file"
    case Expectation.EndOfString(_, _) =>
      "end of file"
    case Expectation.Length(_, expected, actual) =>
      s"unexpected eof; expected ${expected - actual} more characters"
    case Expectation.ExpectedFailureAt(_, matched) =>
      s"unexpected '$matched'"
    case Expectation.Fail(_) =>
      "failed to parse"
    case Expectation.FailWith(_, message) =>
      message
    case Expectation.WithContext(contextStr, _) =>
      s"context: ${escape('\'', contextStr)}"
  }

  def prettyprint(offset: Int, x: NonEmptyList[Parser.Expectation]): String = {
    val (row, col) = locmap.toLineCol(offset).getOrElse((0, 0))
    val (r, c) = (row + 1, col + 1)
    val line: String = locmap.getLine(row).get
    val offending =
      s"${row.toString map { _ => ' ' }} | ${" " * col}^"
    s"""
       |$filename:$r:$c: error: expected one of:
       |    ${x.map(err => description(err)).toList.mkString("\n    ")}
       |$r| $line
       |$offending""".stripMargin
  }

  def prettyprint(x: Parser.Error): String =
    x.expected
      .groupBy(_.offset)
      .map { case (offset, errors) =>
        prettyprint(offset, errors)
      }
      .mkString(", ")

}

object Prettyprint {

  import org.polystat.odin.parser.cats_parse.Parser.`object`

  def main(args: Array[String]): Unit = {
    val code = "1"
    val parsed = `object`(0, 2).parseAll(code)
    println(
      parsed match {
        case Left(value) => new Prettyprint(input = code).prettyprint(value)
        case Right(value) => value
      }
    )
  }

}
