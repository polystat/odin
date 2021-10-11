package org.polystat.odin.parser.cats_parse

import org.polystat.odin.core.ast.{EOBnd, EOProg}
import org.polystat.odin.parser.cats_parse.Tokens._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.parse.{Parser => P, Parser0 => P0}

object Parser {

  def `object`(indent: Int, indentationStep: Int): P[EOBnd[EOExprOnly]] = {
    P.defer(
      Anon.`object`(indent, indentationStep).backtrack |
        Named.`object`(indent, indentationStep)
    )
  }

  def program(indent: Int, indentationStep: Int): P0[EOProg[EOExprOnly]] = (
    Metas.metas ~
      `object`(indent, indentationStep)
        .repSep0(emptyLinesOrComments)
        .surroundedBy(emptyLinesOrComments)
  ).map { case (metas, objs) =>
    EOProg(metas, objs.toVector)
  }

}
