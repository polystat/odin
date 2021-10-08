package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}
import com.github.tarao.nonempty.collection.NonEmpty
import org.polystat.odin.core.ast.{EOBnd, EOBndExpr}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.cats_parse.Tokens._

object Common {

  val nonEmptyErrorMsg: String =
    "Managed to parse zero arguments, where 1 or more were required. This is probably a bug."

  def deeper(
    indent: Int,
    indentationStep: Int
  ): P[Unit] = (
    P.string(" " * (indent + indentationStep))
  )

  def boundAttributes(
    indent: Int,
    indentationStep: Int
  ): P[Vector[EOBndExpr[EOExprOnly]]] =
    eol *> deeper(indent, indentationStep) *>
      Named
        .`object`(indent + indentationStep, indentationStep)
        .repSep0(sep = (eol *> deeper(indent, indentationStep)).void)
        .map(_.toVector)

  def verticalApplicationArgs(
    indent: Int,
    indentationStep: Int
  ): P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = {
    P.char('\n') *> deeper(indent, indentationStep) *>
      Parser
        .`object`(indent + indentationStep, indentationStep)
        .repSep(min = 1, sep = (eol *> deeper(indent, indentationStep)).void)
        .mapFilter(objs => NonEmpty.from(objs.toList.toVector))
  }

}
