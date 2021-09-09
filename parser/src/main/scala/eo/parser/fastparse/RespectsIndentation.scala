package eo.parser.fastparse

import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast.{EOBnd, EOBndExpr, LazyName}
import eo.core.ast.astparams.EOExprOnly
import eo.parser.fastparse.Tokens.singleLineWhitespace
import eo.parser.fastparse.Utils.createNonEmpty
import fastparse._, NoWhitespace._

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

  def args[_: P]: P[(Vector[LazyName], Option[LazyName])] = P(
    "[" ~
      (Tokens.identifier | "@").!.rep(sep = singleLineWhitespace) ~ "...".!.? ~
      "]"
  ).map {
    case (args, None) =>
      (args.map(LazyName).toVector, None)
    case (args, Some(_)) =>
      (args.map(LazyName).toVector.init, Some(LazyName(args.last)))
  }

  def boundAttributes[_: P]: P[Vector[EOBndExpr[EOExprOnly]]] = P(
    ("\n" ~ Tokens.emptyLinesOrComments ~ deeper).flatMap(
      i => new NamedObjects(indent = i).namedObject
        .rep(sep = "\n" ~ Tokens.emptyLinesOrComments ~ (" " * i))
    )
  ).map(_.toVector)

  def verticalApplicationArgs[_: P]
  : P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    ("\n" ~ Tokens.emptyLinesOrComments ~ deeper).flatMap(
      i => new Parser(indent = i).`object`
        .rep(1, sep = "\n" ~ Tokens.emptyLinesOrComments ~ (" " * i))
    )
  ).map(createNonEmpty)


}
