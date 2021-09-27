package org.polystat.odin.parser.fastparse

import com.github.tarao.nonempty.collection.NonEmpty
import fastparse.SingleLineWhitespace._
import fastparse._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

private[parser] object SingleLineApplication {

  def parameterName[_: P]: P[LazyName] = P(
    Tokens.identifier | P("@").map(_ => "@")
  ).map(LazyName)

  def args[_: P]: P[(Vector[LazyName], Option[LazyName])] =
    P(
      "[" ~
        (
          (parameterName.rep(1) ~ "...").map(params =>
            (params.init.toVector, Some(params.last))
          ) |
            parameterName.rep.map(params => (params.toVector, None))
        )
        ~ "]"
    )

  def attributeName[_: P]: P[String] = (Tokens.identifier | "$" | "@" | "^").!

  def data[_: P]: P[EOExprOnly] =
    P(Tokens.integer | Tokens.string | Tokens.char)

  def simpleApplicationTarget[_: P]: P[EOExprOnly] = P(
    data | attributeName.map(name => Fix[EOExpr](EOSimpleApp(name)))
  )

  def attributeChain[_: P]: P[EOExprOnly] = P(
    simpleApplicationTarget ~ "." ~ attributeName.rep(1, sep = ".")
  ).map { case (start, attrs) =>
    attrs.foldLeft(start)((acc, id) => Fix[EOExpr](EODot(acc, id)))
  }

  def applicationTarget[_: P]: P[EOExprOnly] = {
    attributeChain | simpleApplicationTarget
  }

  def parenthesized[_: P]: P[EOExprOnly] = P("(" ~ singleLineApplication ~ ")")

  def horizontalApplicationArgs[
    _: P
  ]: P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = {
    // a workaround suggested by
    // https://github.com/com-lihaoyi/fastparse/issues/217
    def innerHorizontalApplicationArgs = P(
      (applicationTarget | parenthesized).rep(1)
    )
    innerHorizontalApplicationArgs.flatMap(args =>
      NonEmpty.from(args.map(EOAnonExpr(_))) match {
        case Some(value) => Pass(value.toVector)
        case None => Fail(Common.nonEmptyErrorMsg)
      }
    )
  }

  def justApplication[_: P]: P[EOExprOnly] = P(
    (parenthesized | applicationTarget) ~ horizontalApplicationArgs
  ).map { case (trg, args) =>
    Fix[EOExpr](EOCopy(trg, args))
  }

  def singleLineApplication[_: P]: P[EOExprOnly] = P(
    justApplication | parenthesized | applicationTarget | singleLineArray
  )

  def singleLineArray[_: P]: P[EOExprOnly] = P(
    "*" ~ (parenthesized | applicationTarget).rep
  ).map { elems =>
    Fix[EOExpr](
      EOArray(
        elems.map(EOAnonExpr(_)).toVector
      )
    )
  }

}
