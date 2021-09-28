package org.polystat.odin.analysis

import cats.effect.Sync
import cats.implicits.catsSyntaxFoldableOps0
import fs2.Stream
import monix.newtypes.NewtypeWrapped
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.analysis.mutualrec.naive.findMutualRecursionFromAst
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.fastparse.Parser

trait EOOdinAnalyzer[F[_]] {
  def analyzeSourceCode(code: String): Stream[F, OdinAnalysisError]
}

object EOOdinAnalyzer {
  type OdinAnalysisError = OdinAnalysisError.Type

  object OdinAnalysisError extends NewtypeWrapped[String]

  def impl[F[_]: Sync]: EOOdinAnalyzer[F] = new EOOdinAnalyzer[F] {

    def analyzeSourceCode(code: String): Stream[F, OdinAnalysisError] =
      for {
        programAst <- Stream.eval(
          Sync[F].fromEither(
            Parser
              .parse(code)
              .fold(
                onFailure = (label, index, extra) =>
                  Left(
                    new IllegalArgumentException(
                      s"""[$index] Parsing failed with error: $label. Extra:
                         |$extra""".stripMargin
                    )
                  ),
                onSuccess = (ast, _) => Right(ast)
              )
          )
        )
        mutualRecursionErrors <- findMutualRecursion(programAst)
      } yield mutualRecursionErrors

    private def findMutualRecursion(
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
          mutualRecString ++ " through the following possible code path:\n" ++ dependencyChainString
        OdinAnalysisError(errorMessage)
      })
    } yield odinError

  }

}
