package org.polystat.odin.analysis.utils

import cats.ApplicativeError
import cats.data.NonEmptyList
import cats.syntax.all._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

object ImportProcessing {

  case class Importable(
    src: NonEmptyList[String],
    content: EOObj[EOExprOnly]
  )

  def fetchAvailableImportables(src: List[String])(
    bnd: EOBndExpr[EOExprOnly]
  )(implicit pkg: Option[String]): List[Importable] = Fix.un(bnd.expr) match {
    case obj @ EOObj(_, _, bndAttrs) =>
      val currentName = bnd.bndName.name.name
      val newPath = pkg.toList ++ src.appended(currentName)

      List(
        Importable(NonEmptyList.fromListUnsafe(newPath), obj)
      ) ++ bndAttrs.flatMap(fetchAvailableImportables(newPath))
    case _ => List()
  }

  def prependImports[F[_]](
    fileToAst: Map[String, EOProg[EOExprOnly]]
  )(implicit
    ae: ApplicativeError[F, Throwable]
  ): F[Map[String, EOProg[EOExprOnly]]] = {
    val usedImports = fileToAst
      .toList
      .toMap
    val availableImports = for {
      prog <- fileToAst.values
      pkg = prog.metas.pack
      importable <- prog
        .bnds
        .collect { case bnd @ EOBndExpr(_, _) => bnd } flatMap (bnd =>
        fetchAvailableImportables(List())(bnd)(pkg)
      )
    } yield importable

    ae.fromOption(
      usedImports
        .toList
        .traverse { case (fileName, prog @ EOProg(_, _)) =>
          prog
            .metas
            .metas
            .traverse {
              case EOAliasMeta(alias, src) =>
                for {
                  imp <-
                    availableImports
                      .find(imp => imp.src == src)
                  boundObj = imp.content
                  name = alias.getOrElse(src.last)
                } yield EOBndExpr(EOAnyNameBnd(LazyName(name)), Fix(boundObj))
              case _ => None
            }
            .map(importBnds =>
              (fileName, prog.copy(bnds = prog.bnds.prependedAll(importBnds)))
            )
        }
        .map(_.toMap),
      new Exception("Could not find necessary imports after parsing")
    )
  }

}
