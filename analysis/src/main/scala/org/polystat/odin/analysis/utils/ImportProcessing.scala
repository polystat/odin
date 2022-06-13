package org.polystat.odin.analysis.utils

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.syntax.all._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

object ImportProcessing {

  case class Importable(
    pkg: Option[String],
    path: NonEmptyList[String],
    content: EOExprOnly,
    srcFile: String
  ) {
    val fullSrc: NonEmptyList[String] = pkg.map(path.prepend).getOrElse(path)
    val isTopLvl: Boolean = path.length == 1

    def toBndExpr(alias: Option[String] = None): EOBndExpr[EOExprOnly] = {
      val name = alias.getOrElse(path.last)
      EOBndExpr(EOAnyNameBnd(LazyName(name)), content)
    }

  }

  def fetchAvailableImportables(src: List[String])(
    bnd: EOBndExpr[EOExprOnly]
  )(implicit pkg: Option[String], sourceFile: String): List[Importable] = {
    val newPath =
      NonEmptyList.ofInitLast(src, bnd.bndName.name.name)

    bnd.expr match {
      case obj @ Fix(EOObj(_, _, bndAttrs)) =>
        List(
          Importable(pkg, newPath, obj, sourceFile)
        ) ++ bndAttrs.flatMap(fetchAvailableImportables(newPath.toList))
      case other => List(Importable(pkg, newPath, other, sourceFile))
    }
  }

  def fetchImports(
    availableImports: List[Importable]
  )(meta: EOMetas): Option[Vector[EOBndExpr[EOExprOnly]]] = {
    val currentPkg = meta.pack

    val pkgImports = availableImports.collect {
      case imp @ Importable(pkg, _, _, _)
           if pkg == currentPkg && imp.isTopLvl => imp.toBndExpr()
    }

    val nonPkgImports =
      meta
        .metas
        .filter {
          // Todo: check a special case where the pkg name eq the import name
          case EOAliasMeta(_, src) if !currentPkg.contains(src.head) => true
          case _ => false
        }
        .traverse {
          case EOAliasMeta(alias, src) =>
            for {
              imp <-
                availableImports
                  .find(imp => imp.fullSrc == src)
            } yield imp.toBndExpr(alias)
          case _ => None
        }

    nonPkgImports.map(_ ++ pkgImports)
  }

  def prependImports[F[_]](
    fileToAst: Map[String, EOProg[EOExprOnly]]
  )(implicit
    ae: ApplicativeError[F, Throwable]
  ): F[Map[String, EOProg[EOExprOnly]]] = {
    val availableImports = for {
      (file, prog) <- fileToAst.toList
      pkg = prog.metas.pack
      importable <- prog
        .bnds
        .collect { case bnd @ EOBndExpr(_, _) => bnd }
        .flatMap(bnd => fetchAvailableImportables(List())(bnd)(pkg, file))
    } yield importable

    ae.fromOption(
      fileToAst
        .toList
        .traverse { case (fileName, prog @ EOProg(meta, _)) =>
          fetchImports(availableImports)(meta)
            .map(importBnds =>
              (fileName, prog.copy(bnds = prog.bnds.prependedAll(importBnds)))
            )
        }
        .map(_.toMap),
      new Exception("Could not find necessary imports after parsing")
    )
  }

}
