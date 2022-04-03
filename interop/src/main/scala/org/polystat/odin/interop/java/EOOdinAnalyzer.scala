package org.polystat.odin.interop.java

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import fs2.Stream
import org.polystat.odin.analysis
import org.polystat.odin.analysis.ASTAnalyzer
import org.polystat.odin.analysis.EOOdinAnalyzer.{
  advancedMutualRecursionAnalyzer,
  unjustifiedAssumptionAnalyzer,
  accessToBaseClassAnalyzer,
  OdinAnalysisError
}
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.interop.java.OdinAnalysisErrorInterop.fromOdinAnalysisError
import org.polystat.odin.parser.EoParser
import org.polystat.odin.parser.EoParser.sourceCodeEoParser

import java.util
import scala.jdk.CollectionConverters._

trait EOOdinAnalyzer[R] {

  @throws[Exception]
  def analyze(
    eoRepr: R
  ): java.util.List[OdinAnalysisErrorInterop]

}

object EOOdinAnalyzer {

  private val analyzers: List[(String, ASTAnalyzer[IO])] =
    List(
      ("[Mutual Recursion]", advancedMutualRecursionAnalyzer[IO]),
      ("[Unjustified Assumption]", unjustifiedAssumptionAnalyzer[IO]),
      ("[Direct Access To Base Class State]", accessToBaseClassAnalyzer[IO])
    )

  private def runAnalyzers(
    code: String,
    parser: EoParser[String, IO, EOProg[EOExprOnly]]
  )(implicit runtime: IORuntime): util.List[OdinAnalysisErrorInterop] = {
    Stream
      .emits(analyzers)
      .flatMap { case (prefix, analyzer) =>
        analysis
          .EOOdinAnalyzer
          .analyzeSourceCode(analyzer)(code)(parser)
          .map { case OdinAnalysisError(msg) =>
            OdinAnalysisError(List(prefix, msg).mkString(" "))
          }
          .map(fromOdinAnalysisError)
      }
      .compile
      .toList
      .unsafeRunSync()
      .asJava

  }

  class EOOdinSourceCodeAnalyzer() extends EOOdinAnalyzer[String] {

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): java.util.List[OdinAnalysisErrorInterop] =
      runAnalyzers(eoRepr, sourceCodeEoParser())

  }

  class EOOdinXmirAnalyzer() extends EOOdinAnalyzer[String] {

    import org.polystat.odin.parser.EoParser.xmirToEoProgEoParser

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): util.List[OdinAnalysisErrorInterop] =
      runAnalyzers(eoRepr, xmirToEoProgEoParser)

  }

}
