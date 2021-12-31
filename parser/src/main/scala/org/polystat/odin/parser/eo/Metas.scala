package org.polystat.odin.parser.eo

import cats.parse.SemVer.semverString
import cats.parse.{Parser => P, Parser0}
import org.polystat.odin.core.ast.{EOAliasMeta, EOMetas, EORTMeta}
import org.polystat.odin.parser.eo.Tokens._

object Metas {

  private val packageName =
    identifier
      .repSep(1, P.char('.'))
      .string

  val packageMeta: P[String] =
    P.string("+package") *> wsp *> packageName

  private val aliasName = identifier

  val aliasMeta: P[EOAliasMeta] = (
    P.string("+alias") *>
      aliasName.surroundedBy(wsp) ~
      packageName
  ).map { case (alias, src) =>
    EOAliasMeta(alias, src)
  }

  private val artifactId = {

    val artifactName = identifier
    val artifactVersion = semverString

    (
      packageName ~
        artifactName.surroundedBy(P.char(':')) ~
        artifactVersion
    ).string
  }

  val rtMeta: P[EORTMeta] = (
    P.string("+rt") *>
      aliasName.surroundedBy(wsp) ~
      artifactId
  ).map { case (alias, src) =>
    EORTMeta(alias, src)
  }

  val metas: Parser0[EOMetas] = (
    (emptyLinesOrComments *> (packageMeta <* eol).?) ~
      (emptyLinesOrComments.with1.soft *> ((rtMeta | aliasMeta) <* eol)).rep0
  ).map { case (pkg, metas) =>
    EOMetas(pkg, metas.toVector)
  }

}