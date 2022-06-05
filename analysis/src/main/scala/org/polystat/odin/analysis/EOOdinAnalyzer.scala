package org.polystat.odin.analysis

import cats._
import cats.data.EitherNel
import cats.data.NonEmptyList
import cats.syntax.all._
import org.polystat.odin.analysis.EOOdinAnalyzer._
import org.polystat.odin.analysis.liskov.Analyzer
import org.polystat.odin.analysis.mutualrec.advanced.Analyzer.analyzeAst
import org.polystat.odin.analysis.stateaccess.DetectStateAccess
import org.polystat.odin.analysis.utils.inlining.Inliner
import org.polystat.odin.core.ast.EOBndExpr
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EoParser
import org.polystat.odin.parser.eo.Parser

trait ASTAnalyzer[F[_]] {
  val name: String
  def analyze(ast: EOProg[EOExprOnly]): F[OdinAnalysisResult]
}

object EOOdinAnalyzer {

  private def addPredef(existing: EOProg[EOExprOnly]): EOProg[EOExprOnly] = {
    val PREDEF = Seq(
      """
        |[] > class__Object
        |  [] > new
        |    [] > self
        |  [self] > constructor
        |    self > @
        |    [self] > init
        |      [] > @
        |  [self] > init
        |    seq > @
        |      TRUE
        |""".stripMargin
    ).flatMap(code => Parser.parse(code).toOption.get.bnds)
      .filterNot(bnd =>
        existing.bnds.collect { case e @ EOBndExpr(_, _) => e }.contains(bnd)
      )

    existing.copy(bnds = existing.bnds.prependedAll(PREDEF))
  }

  sealed trait OdinAnalysisResult {
    val ruleId: String
  }

  object OdinAnalysisResult {

    final case class Ok(override val ruleId: String) extends OdinAnalysisResult

    final case class DefectsDetected(
      override val ruleId: String,
      messages: NonEmptyList[String],
    ) extends OdinAnalysisResult

    final case class AnalyzerFailure(
      override val ruleId: String,
      reason: Throwable
    ) extends OdinAnalysisResult

    def fromErrors(
      analyzer: String
    )(errors: List[String]): OdinAnalysisResult =
      errors match {
        case e :: es => DefectsDetected(analyzer, NonEmptyList(e, es))
        case Nil => Ok(analyzer)
      }

    def fromThrow[F[_]: ApplicativeThrow](
      analyzer: String
    )(f: F[List[String]]): F[OdinAnalysisResult] =
      f.attempt.map {
        case Left(value) => AnalyzerFailure(analyzer, value)
        case Right(errors) => fromErrors(analyzer)(errors)
      }

  }

  private def toThrow[F[_], A](
    eitherNel: EitherNel[String, A]
  )(implicit mt: MonadThrow[F]): F[A] = {
    MonadThrow[F].fromEither(
      eitherNel
        .leftMap(_.mkString_(util.Properties.lineSeparator))
        .leftMap(new Exception(_))
    )
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

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): F[OdinAnalysisResult] =
        OdinAnalysisResult.fromThrow[F](name) {
          for {
            tree <-
              toThrow(Inliner.zipMethodsWithTheirInlinedVersionsFromParent(ast))
            errors <-
              toThrow(unjustifiedassumptions.Analyzer.analyzeObjectTree(tree))
          } yield errors
        }

    }

  def liskovPrincipleViolationAnalyzer[F[_]: MonadThrow]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override val name: String = "Liskov Substitution principle violation"

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): F[OdinAnalysisResult] =
        OdinAnalysisResult.fromThrow[F](name) {
          for {
            partialTree <-
              toThrow(Inliner.createObjectTree(ast))
            parentedTree <- toThrow(Inliner.resolveParents(partialTree))
            tree <- toThrow(Inliner.resolveIndirectMethods(parentedTree))
            errors <-
              toThrow(Analyzer.analyze(tree))
          } yield errors
        }

    }

  def directStateAccessAnalyzer[F[_]: MonadThrow]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override val name: String = "Direct Access to Superclass State"

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): F[OdinAnalysisResult] =
        OdinAnalysisResult.fromThrow[F](name) {
          for {
            tmpTree <-
              toThrow(Inliner.createObjectTree(ast))
            tree <- toThrow(Inliner.resolveParents(tmpTree))
            errors <-
              toThrow(DetectStateAccess.analyze(tree))
          } yield errors
        }

    }

  def analyzeSourceCode[EORepr, F[_]](
    analyzer: ASTAnalyzer[F]
  )(
    eoRepr: EORepr
  )(implicit
    m: Monad[F],
    parser: EoParser[EORepr, F, EOProg[EOExprOnly]],
  ): F[OdinAnalysisResult] = for {
    programAst <- parser.parse(eoRepr)
    astWithPredef = addPredef(programAst)
    mutualRecursionErrors <-
      analyzer
        .analyze(astWithPredef)
//        .handleErrorWith(_ => Stream.empty)
  } yield mutualRecursionErrors

}
