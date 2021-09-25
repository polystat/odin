package org.polystat.odin.parser.fastparse

import com.github.tarao.nonempty.collection.NonEmpty
import fastparse._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast.{EOBnd, EOBndExpr}
import org.polystat.odin.parser.Utils.createNonEmpty
import org.polystat.odin.parser.fastparse.IgnoreEmptyLinesOrComments._

/**
  * Contains parts required by both named and unnamed objects
  */
private[parser] object Common {

  private def deeper[_: P](indent: Int, indentationStep: Int) = P(
    " " * (indent + indentationStep)
  )

  def boundAttributes[_: P](
    indent: Int,
    indentationStep: Int
  ): P[Vector[EOBndExpr[EOExprOnly]]] = P(
    "\n" ~ deeper(indent, indentationStep) ~~/
      NamedObjects
        .namedObject(indent + indentationStep, indentationStep)
        .repX(sep = "\n" ~ deeper(indent, indentationStep)./)
  ).map(_.toVector)

  def verticalApplicationArgs[_: P](
    indent: Int,
    indentationStep: Int
  ): P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    "\n" ~ deeper(indent, indentationStep) ~~/
      Parser
        .`object`(indent + indentationStep, indentationStep)
        .repX(1, sep = "\n" ~ deeper(indent, indentationStep)./)
  ).map(createNonEmpty)

}
