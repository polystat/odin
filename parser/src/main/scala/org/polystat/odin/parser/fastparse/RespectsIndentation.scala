package org.polystat.odin.parser.fastparse

import com.github.tarao.nonempty.collection.NonEmpty
import fastparse._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast.{EOBnd, EOBndExpr}
import org.polystat.odin.parser.Utils.createNonEmpty
import org.polystat.odin.parser.fastparse.IgnoreEmptyLinesOrComments._
/**
 * The base class for both types of objects.
 * It contains some indentation handling parsers (like deeper) and
 * parsers that are required by both types of objects (both named and anonymous)
 *
 * @param indent          the spaces before the statements in the outermost block
 * @param indentationStep InnerBlockIndentation minus OuterBlockIndentation
 */
abstract class RespectsIndentation(
                                    val indent: Int = 0,
                                    val indentationStep: Int = 2
                                  ) {

  def deeper[_: P]: P[Int] = P(
    (" " * (indent + indentationStep)).!
  ).map(_.length)

  def boundAttributes[_: P]: P[Vector[EOBndExpr[EOExprOnly]]] = P(
    ("\n" ~ deeper).flatMapX(
      i => new NamedObjects(indent = i).namedObject
        .repX(sep = "\n" ~ (" " * i))
    )
  ).map(_.toVector)

  def verticalApplicationArgs[_: P]
  : P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    ("\n" ~ deeper).flatMapX(
      i => new Parser(indent = i).`object`
        .repX(1, sep = "\n" ~ (" " * i))
    )
  ).map(createNonEmpty)


}
