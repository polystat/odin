package org.polystat.odin.interop.java

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.polystat.odin.analysis
import org.polystat.odin.interop.java.OdinAnalysisErrorInterop.fromOdinAnalysisError
import org.polystat.odin.analysis.EOOdinAnalyzer.advancedMutualRecursionAnalyzer
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

  class EOOdinSourceCodeAnalyzer() extends EOOdinAnalyzer[String] {

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): java.util.List[OdinAnalysisErrorInterop] =
      analysis
        .EOOdinAnalyzer
        .analyzeSourceCode[String, IO](advancedMutualRecursionAnalyzer)(eoRepr)(
          sourceCodeEoParser()
        )
        .map(fromOdinAnalysisError)
        .compile
        .toList
        .unsafeRunSync()
        .asJava

  }

  class EOOdinXmirAnalyzer() extends EOOdinAnalyzer[String] {

    import org.polystat.odin.parser.EoParser.{
      xmirToEoBndEoParser,
      xmirToEoProgEoParser
    }
    import org.polystat.odin.parser.xmir.XmirToAst.string

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): util.List[OdinAnalysisErrorInterop] =
      analysis
        .EOOdinAnalyzer
        .analyzeSourceCode[String, IO](advancedMutualRecursionAnalyzer)(eoRepr)(
          xmirToEoProgEoParser
        )
        .map(fromOdinAnalysisError)
        .compile
        .toList
        .unsafeRunSync()
        .asJava

  }

}
