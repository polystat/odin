package org.polystat.odin.parser.fastparse

import fastparse._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import IgnoreEmptyLinesOrComments._
import org.polystat.odin.parser.fastparse.Metas.{aliasMeta, packageMeta, rtMeta}


@deprecated("Use org.polystat.odin.parser.cats_parse package instead", "0.1.5")
object Parser {

  private[parser] def metas[_: P]: P[EOMetas] = P(
    packageMeta.? ~/
      (rtMeta | aliasMeta)./.rep
  ).map { case (pkg, metas) =>
    EOMetas(pkg, metas.toVector)
  }

  private[parser] def program[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOProg[EOExprOnly]] = P(
    Start ~/
      metas ~/
      `object`(indent, indentationStep)./.rep ~
      End
  ).map { case (metas, objs) =>
    EOProg(
      metas = metas,
      bnds = objs.toVector
    )
  }

  private[parser] def `object`[_: P](
    indent: Int,
    indentationStep: Int
  ): P[EOBnd[EOExprOnly]] = P(
    NamedObjects.namedObject(indent, indentationStep) |
      AnonymousObjects.anonymousObject(indent, indentationStep)
  )

  def parse(
    code: String,
    indentationStep: Int = 2
  ): Parsed[EOProg[EOExprOnly]] = {
    fastparse.parse(code, program(0, indentationStep)(_))
  }

}
