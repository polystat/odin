package org.polystat.odin.parser.fastparse

import fastparse.SingleLineWhitespace._
import fastparse._
import org.polystat.odin.core.ast.{EOAliasMeta, EORTMeta}

object Metas {
  def packageMeta[_: P]: P[String] = P(
    "+package" ~ packageName.! ~ "\n"
  )

  def aliasMeta[_: P]: P[EOAliasMeta] = P(
    "+alias" ~ aliasName.! ~ packageName.! ~ "\n"
  ).map {
    case (alias, src) => EOAliasMeta(alias, src)
  }

  def rtMeta[_: P]: P[EORTMeta] = P(
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

}
