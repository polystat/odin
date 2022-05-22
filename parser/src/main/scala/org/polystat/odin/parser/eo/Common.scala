package org.polystat.odin.parser.eo

import cats.parse.{Parser => P}
import com.github.tarao.nonempty.collection.NonEmpty
import org.polystat.odin.core.ast.EOBnd
import org.polystat.odin.core.ast.EOBndExpr
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Tokens._

object Common {

  val nonEmptyErrorMsg: String =
    "Managed to parse zero arguments, where 1 or more were required. This is probably a bug."

  def deeper(
    indent: Int,
    indentationStep: Int
  ): P[Unit] = P.string(" " * (indent + indentationStep))

  def wspBetweenObjs(
    indent: Int,
    indentationStep: Int
  ): P[Unit] = {
    (eol *> emptyLinesOrComments).soft *>
      deeper(indent, indentationStep)
  }

  def boundAttributes(
    indent: Int,
    indentationStep: Int
  ): P[Vector[EOBndExpr[EOExprOnly]]] = P.defer(
    wspBetweenObjs(indent, indentationStep)
      *> Named
        .`object`(indent + indentationStep, indentationStep)
        .repSep(1, sep = wspBetweenObjs(indent, indentationStep))
        .map(_.toList.toVector)
  )

  def verticalApplicationArgs(
    indent: Int,
    indentationStep: Int
  ): P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P.defer(
    wspBetweenObjs(indent, indentationStep)
      *> Parser
        .`object`(indent + indentationStep, indentationStep)
        .repSep(1, wspBetweenObjs(indent, indentationStep))
        .mapFilter(objs => NonEmpty.from(objs.toList.toVector))
  )

}
