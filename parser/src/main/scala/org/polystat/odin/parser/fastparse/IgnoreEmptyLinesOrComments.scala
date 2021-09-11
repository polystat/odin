package org.polystat.odin.parser.fastparse

import fastparse._

object IgnoreEmptyLinesOrComments {
  implicit val whitespace: P[_] => P[Unit] = { implicit ctx: ParsingRun[_] =>
    Tokens.emptyLinesOrComments
  }
}
