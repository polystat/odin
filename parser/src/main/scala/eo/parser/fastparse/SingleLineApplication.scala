package eo.parser.fastparse

import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast.{EOAnonExpr, EOBnd, EOCopy, EODot, EOExpr, EOSimpleApp}
import eo.core.ast.astparams.EOExprOnly
import eo.parser.fastparse.Utils.createNonEmpty
import fastparse._, SingleLineWhitespace._
import higherkindness.droste.data.Fix

object SingleLineApplication {

  def attributeName[_: P]: P[String] = (Tokens.identifier | "$" | "@" | "^").!

  def data[_: P]: P[EOExprOnly] = P(Tokens.integer | Tokens.string | Tokens.char)

  def simpleApplicationTarget[_: P]: P[EOExprOnly] = P(
    data | attributeName.map(name => Fix[EOExpr](EOSimpleApp(name)))
  )

  def attributeChain[_: P]: P[EOExprOnly] = P(
    simpleApplicationTarget ~ "." ~ attributeName.rep(1, sep = ".")
  ).map {
    case (start, attrs) =>
      attrs.foldLeft(start)((acc, id) => Fix[EOExpr](EODot(acc, id)))
  }

  def applicationTarget[_: P]: P[EOExprOnly] = {
    attributeChain | simpleApplicationTarget
  }

  def parenthesized[_: P]: P[EOExprOnly] = P("(" ~ singleLineApplication ~ ")")

  def horizontalApplicationArgs[_: P]
  : P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = P(
    (applicationTarget | parenthesized).rep(1)
  ).map(args => createNonEmpty(args.map(EOAnonExpr(_))))

  def justApplication[_: P]: P[EOExprOnly] = P(
    (parenthesized | applicationTarget) ~ horizontalApplicationArgs
  ).map {
    case (trg, args) => Fix[EOExpr](EOCopy(trg, args))
  }

  def singleLineApplication[_: P]: P[EOExprOnly] = P(
    justApplication | parenthesized | applicationTarget
  )
}
