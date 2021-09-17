package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}
import org.polystat.odin.core.ast._

object SingleLine {

  lazy val parameterName: P[LazyName] = (
    Tokens.identifier | P.char('@').as("@")
    ).map(LazyName)

  lazy val params: P[(Vector[LazyName], Option[LazyName])] = (
    P.string("[") *>
      (
        (parameterName.repSep(1, Tokens.singleLineWhitespace).soft <* P.string("..."))
          .map(params => (params.init.toVector, Some(params.last))) |
          parameterName.repSep0(0, Tokens.singleLineWhitespace)
            .map(params => (params.toVector, None))
        )
      <* P.string("]")
    )
}
