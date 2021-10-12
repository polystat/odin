package org.polystat.odin.parser.cats_parse

import org.polystat.odin.core.ast.{EOBnd, EOProg}
import org.polystat.odin.parser.cats_parse.Tokens._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.parse.{Parser => P, Parser0 => P0}

object Parser {

  def `object`(indent: Int, indentationStep: Int): P[EOBnd[EOExprOnly]] = {
    P.defer(
      Named.`object`(indent, indentationStep) |
        Anon.`object`(indent, indentationStep)
    )
  }

  def program(indent: Int, indentationStep: Int): P0[EOProg[EOExprOnly]] = (
    Metas.metas ~
      (emptyLinesOrComments *>
        `object`(indent, indentationStep)
          .repSep0(emptyLinesOrComments)) <*
      emptyLinesOrComments
  ).map { case (metas, objs) =>
    EOProg(metas, objs.toVector)
  }

}
