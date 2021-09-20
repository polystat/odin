package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.cats_parse.Tokens._

object SingleLine {

  val parameterName: P[LazyName] = (
    Tokens.identifier | P.stringIn("@" :: Nil)
    ).map(LazyName)

  val params: P[(Vector[LazyName], Option[LazyName])] = (
    P.charIn('[') *>
      (
        (parameterName.repSep(1, wsp).soft <* P.string("..."))
          .map(params => (params.init.toVector, Some(params.last))) |
          parameterName.repSep0(0, wsp)
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

  val simpleApplicationTarget: P[EOExprOnly] =
    data | attributeName.map(name => Fix[EOExpr](EOSimpleApp(name)))
}
