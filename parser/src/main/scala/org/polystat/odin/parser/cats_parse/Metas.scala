package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser0, Parser => P}
import org.polystat.odin.core.ast.{EOAliasMeta, EOMetas, EORTMeta}

object Metas {

  private lazy val packageName = {
    Tokens.identifier.repSep(1, P.char('.'))
      .map(_.toList.mkString("."))
  }

  lazy val packageMeta: P[String] =
    P.string("+package") *> Tokens.singleLineWhitespace *> packageName

  private lazy val aliasName = Tokens.identifier

  lazy val aliasMeta: P[EOAliasMeta] = (
    P.string("+alias") *>
      aliasName.surroundedBy(Tokens.singleLineWhitespace) ~
        packageName
    ).map {
    case (alias, src) => EOAliasMeta(alias, src)
  }


  private lazy val artifactId = {

    lazy val artifactName = Tokens.identifier

    lazy val artifactVersion =
      Tokens.digit.rep(1).repSep(3, P.string("."))
        .map {
          lst =>
            lst.map(_.toList.mkString)
              .toList.mkString(".")
        }

    (
      packageName ~
        artifactName.surroundedBy(P.char(':')) ~
        artifactVersion
      ).map {
      case ((pkgName, id), version) => (pkgName :: id :: version :: Nil).mkString(":")
    }
  }

  lazy val rtMeta: P[EORTMeta] = (
    P.string("+rt") *>
      aliasName.surroundedBy(Tokens.singleLineWhitespace) ~
        artifactId
    ).map {
    case (alias, src) => EORTMeta(alias, src)
  }

  lazy val metas: Parser0[EOMetas] = (
    (Tokens.emptyLinesOrComments *> (packageMeta <* Tokens.eol).?) ~
      (Tokens.emptyLinesOrComments.with1 *> ((rtMeta | aliasMeta) <* Tokens.eol)).rep0
    ).map {
    case (pkg, metas) => EOMetas(pkg, metas.toVector)
  }
}
