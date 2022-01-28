package org.polystat.odin.parser.eo

import cats.parse.{Parser => P}
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Tokens._
import com.github.tarao.nonempty.collection.NonEmpty

object SingleLine {

  val parameterName: P[LazyName] = (
    Tokens.identifier | P.string("@").string
  ).map(LazyName)

  val params: P[(Vector[LazyName], Option[LazyName])] = (
    P.charIn('[') *>
      (
        (parameterName.repSep(1, wsp) <* P.string("..."))
          .backtrack
          .map(params => (params.init.toVector, Some(params.last))) |
          parameterName
            .repSep0(0, wsp)
            .map(params => (params.toVector, None))
      )
      <* P.charIn(']')
  )

  val bndName: P[EONamedBnd] = (
    (optWsp.with1.soft *> P.char('>') *> optWsp) *>
      ((identifier | P.string("@").string) <* optWsp) ~
      (P.charIn('!') <* optWsp).?
  ).map {
    case ("@", _) => EODecoration
    case (name, Some(_)) => EOAnyNameBnd(ConstName(name))
    case (name, None) => EOAnyNameBnd(LazyName(name))
  }

  val attributeName: P[String] =
    identifier |
      P.string("@").string |
      P.string("$").string |
      P.string("^").string

  val data: P[EOExprOnly] = (
    float.map(EOFloatData(_)) |
      integer.map(EOIntData(_)) |
      char.map(EOCharData(_)) |
      string.map(EOStrData(_))
  ).map(Fix[EOExpr](_))

  val singleLineApplication: P[EOExprOnly] =
    P.recursive[EOExprOnly](recurse => {

      val simpleApplicationTarget: P[EOExprOnly] =
        data | attributeName.map(name => Fix[EOExpr](EOSimpleApp(name)))

      val parenthesized: P[EOExprOnly] =
        recurse.between(P.char('('), P.char(')'))

      val singleLineBndExpr: P[EOBndExpr[EOExprOnly]] =
        (recurse ~ bndName)
          .between(P.char('('), P.char(')'))
          .map { case (expr, name) =>
            EOBndExpr(name, expr)
          }

      val singleLineAbstraction: P[EOExprOnly] = (
        params.soft ~ (wsp.soft *> singleLineBndExpr.repSep(wsp))
      ).map { case ((freeAttrs, vararg), bnds) =>
        Fix(
          EOObj(
            freeAttrs,
            vararg,
            bnds.toList.toVector
          )
        )
      }

      val attributeChain: P[EOExprOnly] = (
        ((parenthesized | simpleApplicationTarget).soft <* P.char('.')).soft ~
          attributeName.repSep(1, P.char('.'))
      ).map { case (trg, attrs) =>
        attrs.foldLeft(trg)((acc, id) => Fix[EOExpr](EODot(acc, id)))
      }

      val applicationTarget: P[EOExprOnly] =
        attributeChain | parenthesized | simpleApplicationTarget

      val justApplication: P[EOExprOnly] = (
        (applicationTarget.soft <* wsp).soft ~
          (applicationTarget.map(EOAnonExpr(_)) | singleLineBndExpr)
            .repSep(1, wsp)
      ).map { case (trg, args) =>
        NonEmpty
          .from(args.toList)
          .map(args => Fix[EOExpr](EOCopy(trg, args.toVector)))
          .getOrElse(trg)
      }

      val singleLineArray: P[EOExprOnly] = (
        P.char('*') *> optWsp *>
          applicationTarget.repSep0(0, wsp)
      ).map { elems =>
        Fix[EOExpr](
          EOArray(
            elems.map(EOAnonExpr(_)).toVector
          )
        )
      }

      singleLineArray |
        singleLineAbstraction |
        justApplication |
        applicationTarget

    })

}
