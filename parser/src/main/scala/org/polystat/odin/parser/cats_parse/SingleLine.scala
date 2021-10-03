package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.cats_parse.Tokens._
import com.github.tarao.nonempty.collection.NonEmpty

object SingleLine {

  val nonEmptyErrorMsg: String =
    "Managed to parse zero arguments, where 1 or more were required. This is probably a bug."

  val parameterName: P[LazyName] = (
    Tokens.identifier | P.stringIn("@" :: Nil)
  ).map(LazyName)

  val params: P[(Vector[LazyName], Option[LazyName])] = (
    P.charIn('[') *>
      (
        (parameterName.repSep(1, wsp).soft <* P.string("..."))
          .map(params => (params.init.toVector, Some(params.last))) |
          parameterName
            .repSep0(0, wsp)
            .map(params => (params.toVector, None))
      )
      <* P.charIn(']')
  )

  val bndName: P[EONamedBnd] = (
    P.char('>').surroundedBy(optWsp) *>
      ((identifier | P.stringIn("@" :: Nil)) <* optWsp) ~
      (P.charIn('!') <* optWsp).?
  ).map {
    case ("@", _) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  val attributeName: P[String] =
    identifier |
      P.stringIn("@" :: Nil) |
      P.stringIn("$" :: Nil) |
      P.stringIn("^" :: Nil)

  val data: P[EOExprOnly] = (
    integer.backtrack.map(EOIntData(_)) |
      float.map(EOFloatData(_)) |
      char.map(EOCharData(_)) |
      string.map(EOStrData(_))
  ).map(Fix[EOExpr](_))

  val singleLineApplication: P[EOExprOnly] =
    P.recursive[EOExprOnly](recurse => {

      val simpleApplicationTarget: P[EOExprOnly] =
        data | attributeName.map(name => Fix[EOExpr](EOSimpleApp(name)))

      val attributeChain: P[EOExprOnly] = (
        (simpleApplicationTarget <* P.char('.')) ~
          attributeName.repSep(1, P.char('.'))
      ).map { case (trg, attrs) =>
        attrs.foldLeft(trg)((acc, id) => Fix[EOExpr](EODot(acc, id)))
      }

      val applicationTarget: P[EOExprOnly] =
        attributeChain.backtrack | simpleApplicationTarget

      val parenthesized: P[EOExprOnly] =
        P.char('(') *> recurse <* P.char(')')

      val horizontalApplicationArgs: P[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] =
        (applicationTarget | parenthesized)
          .repSep(1, wsp)
          .flatMap(args =>
            NonEmpty
              .from(args.toList.map(EOAnonExpr(_)).toVector)
              .map(P.pure)
              .getOrElse(P.failWith(nonEmptyErrorMsg))
          )

      val justApplication: P[EOExprOnly] = (
        ((parenthesized | applicationTarget) <* wsp) ~ horizontalApplicationArgs
      ).map { case (trg, args) =>
        Fix[EOExpr](EOCopy(trg, args))
      }

      val singleLineArray: P[EOExprOnly] = (
        P.char('*') *> wsp *> (parenthesized | applicationTarget)
          .repSep0(0, wsp)
      ).map { elems =>
        Fix[EOExpr](
          EOArray(elems.map(EOAnonExpr(_)).toVector)
        )
      }

      justApplication.backtrack | applicationTarget | parenthesized | singleLineArray

    })

}
