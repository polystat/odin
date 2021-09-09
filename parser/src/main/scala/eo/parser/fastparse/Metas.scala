package eo.parser.fastparse

import eo.core.ast.{EOAliasMeta, EOMetas, EORTMeta}
import fastparse._, SingleLineWhitespace._

object Metas {
  private def packageMeta[_: P] = P(
    "+package" ~ packageName.! ~ "\n"
  )

  private def aliasMeta[_: P] = P(
    "+alias" ~ aliasName.! ~ packageName.! ~ "\n"
  ).map {
    case (alias, src) => EOAliasMeta(alias, src)
  }

  private def rtMeta[_: P] = P(
    "+rt" ~ aliasName.! ~ artifactId.! ~ "\n"
  ).map {
    case (rtName, src) => EORTMeta(rtName, src)
  }

  // TODO: refine rule
  private def aliasName[_: P] = P(Tokens.identifier)

  // TODO: refine rule
  private def artifactName[_: P] = P(Tokens.identifier)

  // TODO: refine rule
  private def artifactVersion[_: P] =
    P(Tokens.digit.rep(1).rep(3, sep = "."))
      .map(_.mkString(sep = "."))

  // TODO: refine rule
  private def artifactId[_: P] = P(
    packageName ~ ":" ~ artifactName ~ ":" ~ artifactVersion
  ).map(_.productIterator.mkString(":"))

  // TODO: refine rule
  private def packageName[_: P] = P(
    Tokens.identifier.rep(sep = ".", min = 1)
  ).map(_.mkString(sep = "."))

  def metas[_: P]: P[EOMetas] = P(
    Tokens.emptyLinesOrComments ~ packageMeta.? ~
      (Tokens.emptyLinesOrComments ~ (rtMeta | aliasMeta)).rep
  ).map {
    case (pkg, metas) => EOMetas(pkg, metas.toVector)
  }
}
