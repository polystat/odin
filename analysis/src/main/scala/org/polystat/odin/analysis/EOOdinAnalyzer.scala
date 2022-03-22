package org.polystat.odin.analysis

import cats._
import cats.data.EitherNel
import cats.effect.Sync
import cats.syntax.all._
import fs2.Stream
import org.polystat.odin.analysis.inlining.Inliner
import org.polystat.odin.analysis.logicalexprs.ExtractLogic
import org.polystat.odin.analysis.mutualrec.advanced.Analyzer.analyzeAst
import org.polystat.odin.analysis.mutualrec.naive.findMutualRecursionFromAst
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EoParser
import EOOdinAnalyzer._

trait ASTAnalyzer[F[_]] {
  val name: String
  def analyze(ast: EOProg[EOExprOnly]): F[OdinAnalysisResult]
}

object EOOdinAnalyzer {

  sealed trait OdinAnalysisResult {
    val analysisName: String
  }

  object OdinAnalysisResult {

    final case class Ok(override val analysisName: String)
      extends OdinAnalysisResult

    final case class DefectDetected(
      override val analysisName: String,
      messages: List[String]
    ) extends OdinAnalysisResult

    final case class AnalyzerFailure(
      override val analysisName: String,
      reason: String
    ) extends OdinAnalysisResult

    def fromErrors(
      analyzer: String
    )(errors: List[String]): OdinAnalysisResult =
      if (errors.isEmpty)
        Ok(analyzer)
      else
        DefectDetected(analyzer, errors)

    def fromThrow[F[_]: ApplicativeThrow](
      analyzer: String
    )(f: F[List[String]]): F[OdinAnalysisResult] =
      f.attempt.map {
        case Left(value) => AnalyzerFailure(analyzer, value.getMessage)
        case Right(errors) => fromErrors(analyzer)(errors)
      }

  }

  def naiveMutualRecursionAnalyzer[F[_]: Sync]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override val name: String = "Mutual Recursion (naive)"

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): F[OdinAnalysisResult] = {
        val stream = for {
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
            errorMessage
          })
        } yield odinError

        stream.compile.toList.map(OdinAnalysisResult.fromErrors(name))
      }

    }

  def advancedMutualRecursionAnalyzer[
    F[_]: MonadThrow,
  ]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override val name: String = "Mutual Recursion"

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): F[OdinAnalysisResult] =
        OdinAnalysisResult.fromThrow(name) {
          MonadThrow[F].fromEither(
            analyzeAst[Either[String, *]](ast).leftMap(new Exception(_))
          )
        }

    }

  def unjustifiedAssumptionAnalyzer[F[_]: MonadThrow]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override val name: String = "Unjustified Assumption"

      private def toThrow[A](eitherNel: EitherNel[String, A]): F[A] = {
        MonadThrow[F].fromEither(
          eitherNel
            .leftMap(_.mkString_(util.Properties.lineSeparator))
            .leftMap(new Exception(_))
        )
      }

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): F[OdinAnalysisResult] =
        OdinAnalysisResult.fromThrow[F](name) {
          for {
            tree <-
              toThrow(Inliner.zipMethodsWithTheirInlinedVersionsFromParent(ast))
            errors <-
              toThrow(ExtractLogic.processObjectTree(tree))
          } yield errors
        }

    }

  def analyzeSourceCode[EORepr, F[_]: Monad](
    analyzer: ASTAnalyzer[F]
  )(
    eoRepr: EORepr
  )(implicit
    parser: EoParser[EORepr, F, EOProg[EOExprOnly]]
  ): F[OdinAnalysisResult] = for {
    programAst <- parser.parse(eoRepr)
    mutualRecursionErrors <-
      analyzer
        .analyze(programAst)
//        .handleErrorWith(_ => Stream.empty)
  } yield mutualRecursionErrors

}
