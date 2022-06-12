package org.polystat.odin.analysis.utils

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.syntax.all._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

object ImportProcessing {

  case class Importable(
    src: NonEmptyList[String],
    content: EOExprOnly
  )

  def fetchAvailableImportables(src: List[String])(
    bnd: EOBndExpr[EOExprOnly]
  )(implicit pkg: Option[String]): List[Importable] = {
    val newPath =
      NonEmptyList.ofInitLast(src, bnd.bndName.name.name)
    val fullPath = pkg.map(newPath.prepend).getOrElse(newPath)

    bnd.expr match {
      case obj @ Fix(EOObj(_, _, bndAttrs)) =>
        List(
          Importable(fullPath, obj)
        ) ++ bndAttrs.flatMap(fetchAvailableImportables(newPath.toList))
      case other => List(Importable(fullPath, other))
    }
  }

  def prependImports[F[_]](
    fileToAst: Map[String, EOProg[EOExprOnly]]
  )(implicit
    ae: ApplicativeError[F, Throwable]
  ): F[Map[String, EOProg[EOExprOnly]]] = {
    val availableImports = for {
      prog <- fileToAst.values
      pkg = prog.metas.pack
      importable <- prog
        .bnds
        .collect { case bnd @ EOBndExpr(_, _) => bnd } flatMap (bnd =>
        fetchAvailableImportables(List())(bnd)(pkg)
      )
    } yield importable

    def attemptFetchImport: EOMeta => Option[EOBndExpr[EOExprOnly]] = {
      case EOAliasMeta(alias, src) =>
        for {
          imp <-
            availableImports
              .find(imp => imp.src == src)
          boundObj = imp.content
          name = alias.getOrElse(src.last)
        } yield EOBndExpr(EOAnyNameBnd(LazyName(name)), boundObj)
      case _ => None
    }

    ae.fromOption(
      fileToAst
        .toList
        .traverse { case (fileName, prog @ EOProg(EOMetas(_, metas), _)) =>
          metas
            .traverse(attemptFetchImport)
            .map(importBnds =>
              (fileName, prog.copy(bnds = prog.bnds.prependedAll(importBnds)))
            )
        }
        .map(_.toMap),
      new Exception("Could not find necessary imports after parsing")
    )
  }

}
