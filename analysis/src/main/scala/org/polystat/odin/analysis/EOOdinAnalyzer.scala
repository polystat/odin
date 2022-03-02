package org.polystat.odin.analysis

import cats.ApplicativeThrow
import cats.data.EitherNel
import cats.effect.Sync
import cats.syntax.either._
import cats.syntax.foldable._
import fs2.Stream
import monix.newtypes.NewtypeWrapped
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.analysis.inlining.Inliner
import org.polystat.odin.analysis.logicalexprs.ExtractLogic
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

  def advancedMutualRecursionAnalyzer[F[_]: ApplicativeThrow]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] = for {
        errors <- Stream.eval(
          ApplicativeThrow[F].fromEither(
            analyzeAst[Either[String, *]](ast).leftMap(new Exception(_))
          )
        )
        error <- Stream.emits(errors)
      } yield error

    }

  def unjustifiedAssumptionAnalyzer[F[_]: ApplicativeThrow]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      private[this] def toThrow[A](eitherNel: EitherNel[String, A]): F[A] = {
        ApplicativeThrow[F].fromEither(
          eitherNel
            .leftMap(_.mkString_(util.Properties.lineSeparator))
            .leftMap(new Exception(_))
        )
      }

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] =
        Stream.evals {
          toThrow {
            for {
              tree <- Inliner.zipMethodsWithTheirInlinedVersionsFromParent(ast)
              errors <- ExtractLogic.processObjectTree(tree)
            } yield errors.map(OdinAnalysisError.apply)
          }
        }

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
