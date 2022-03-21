package org.polystat.odin.interop.java

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all._
import org.polystat.odin.analysis
import org.polystat.odin.analysis.ASTAnalyzer
import org.polystat.odin.analysis.EOOdinAnalyzer.{
  advancedMutualRecursionAnalyzer,
  unjustifiedAssumptionAnalyzer
}
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EoParser
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.interop.java.OdinAnalysisResultInterop
import scala.jdk.CollectionConverters._

import java.util

trait EOOdinAnalyzer[R] {

  @throws[Exception]
  def analyze(
    eoRepr: R
  ): java.util.List[OdinAnalysisResultInterop]

}

object EOOdinAnalyzer {

  private val analyzers: List[ASTAnalyzer[IO]] =
    List(
      advancedMutualRecursionAnalyzer[IO],
      unjustifiedAssumptionAnalyzer[IO],
    )

  private def runAnalyzers(
    code: String,
    parser: EoParser[String, IO, EOProg[EOExprOnly]]
  )(implicit runtime: IORuntime): util.List[OdinAnalysisResultInterop] = {
    analyzers
      .flatTraverse { analyzer =>
        analysis
          .EOOdinAnalyzer
          .analyzeSourceCode(analyzer)(code)(cats.Monad[IO], parser)
          .map(OdinAnalysisResultInterop.fromOdinAnalysisResult)
      }
      .unsafeRunSync()
      .asJava
  }

  class EOOdinSourceCodeAnalyzer() extends EOOdinAnalyzer[String] {

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): java.util.List[OdinAnalysisResultInterop] =
      runAnalyzers(eoRepr, sourceCodeEoParser())

  }

  class EOOdinXmirAnalyzer() extends EOOdinAnalyzer[String] {

    import org.polystat.odin.parser.EoParser.xmirToEoProgEoParser

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): java.util.List[OdinAnalysisResultInterop] =
      runAnalyzers(eoRepr, xmirToEoProgEoParser)

  }

}
