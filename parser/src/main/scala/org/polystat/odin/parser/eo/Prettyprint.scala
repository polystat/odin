package org.polystat.odin.parser.eo

// Borrowed from:
// https://github.com/typelevel/cats-parse/issues/239
// In particular, from:
// https://gist.github.com/re-xyr/b0f1988e744c93d4c83a5f17f58484ea

import cats.data.NonEmptyList
import cats.parse._
import cats.parse.{Parser => P}
import org.polystat.odin.utils.text.escape

class Prettyprint(filename: String = "", input: String) {
  private val locmap = LocationMap(input)

  def description(x: P.Expectation): String = x match {
    case P.Expectation.OneOfStr(_, strs) =>
      val strList = strs.map { x => s"'${escape('\'', x)}'" }.mkString(", ")
      s"one of $strList"
    case P.Expectation.InRange(_, lower, upper) =>
      if (lower == upper) s"'${escape('\'', lower.toString)}'"
      else s"'$lower' ~ '$upper'"
    case P.Expectation.StartOfString(_) =>
      "beginning of file"
    case P.Expectation.EndOfString(_, _) =>
      "end of file"
    case P.Expectation.Length(_, expected, actual) =>
      s"unexpected eof; expected ${expected - actual} more characters"
    case P.Expectation.ExpectedFailureAt(_, matched) =>
      s"unexpected '$matched'"
    case P.Expectation.Fail(_) =>
      "failed to parse"
    case P.Expectation.FailWith(_, message) =>
      message
    case P.Expectation.WithContext(contextStr, _) =>
      s"context: ${escape('\'', contextStr)}"
  }

  def prettyprint(offset: Int, x: NonEmptyList[P.Expectation]): String = {
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

  def prettyprint(x: P.Error): String =
    x.expected
      .groupBy(_.offset)
      .map { case (offset, errors) =>
        prettyprint(offset, errors)
      }
      .mkString(", ")

}

object Prettyprint {

  import org.polystat.odin.parser.eo.Parser.`object`

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
