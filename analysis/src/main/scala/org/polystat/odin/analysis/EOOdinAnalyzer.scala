package org.polystat.odin.analysis

import cats.effect.Sync
import cats.implicits.catsSyntaxFoldableOps0
import cats.syntax.either._
import fs2.Stream
import monix.newtypes.NewtypeWrapped
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.analysis.mutualrec.advanced.Analyzer.analyzeAst
import org.polystat.odin.analysis.mutualrec.naive.findMutualRecursionFromAst
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EoParser

trait ASTAnalyzer[F[_]] {
  def analyze(ast: EOProg[EOExprOnly]): Stream[F, OdinAnalysisError]
}

object EOOdinAnalyzer {
  type OdinAnalysisError = OdinAnalysisError.Type
  object OdinAnalysisError extends NewtypeWrapped[String]

  def naiveMutualRecursionAnalyzer[F[_]: Sync]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] = for {
        recursiveDependency <- Stream.evals(findMutualRecursionFromAst(ast))
        (method, depChains) <- Stream.emits(recursiveDependency.toVector)
        depChain <- Stream.emits(depChains.toVector)
        odinError <- Stream.fromOption(for {
          mutualRecMeth <- depChain.lastOption
        } yield {
          val mutualRecString =
            s"Method `${method.parentObject.objName}.${method.name}` " ++
              s"is mutually recursive with method " ++
              s"`${mutualRecMeth.parentObject.objName}.${mutualRecMeth.name}`"

          val dependencyChainString = depChain
            .append(method)
            .map(m => s"${m.parentObject.objName}.${m.name}")
            .mkString_(" -> ")

          val errorMessage =
            mutualRecString ++ " through the following possible code path:\n" ++
              dependencyChainString
          OdinAnalysisError(errorMessage)
        })
      } yield odinError

    }

  def advancedMutualRecursionAnalyzer[F[_]: Sync]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] = for {
        errors <- Stream.eval(
          Sync[F].fromEither(analyzeAst(ast).leftMap(new Exception(_)))
        )
        error <- Stream.emits(errors)
      } yield error

    }

  def analyzeSourceCode[EORepr, F[_]](analyzer: ASTAnalyzer[F])(
    eoRepr: EORepr
  )(implicit
    parser: EoParser[EORepr, F, EOProg[EOExprOnly]]
  ): Stream[F, OdinAnalysisError] = for {
    programAst <- Stream.eval(parser.parse(eoRepr))
    mutualRecursionErrors <- analyzer.analyze(programAst)
  } yield mutualRecursionErrors

}
