package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P}

object Common {

  val nonEmptyErrorMsg: String =
    "Managed to parse zero arguments, where 1 or more were required. This is probably a bug."

  def deeper(
    indent: Int,
    indentationStep: Int
  ): P[Unit] = (
    P.string(" " * (indent + indentationStep))
  )

}
