package org.polystat.odin.interop.java

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.polystat.odin.analysis
import org.polystat.odin.interop.java.OdinAnalysisErrorInterop.fromOdinAnalysisError

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
    import org.polystat.odin.parser.EoParser.sourceCodeEoParser

    private val delegate = analysis
      .EOOdinAnalyzer
      .impl[String, IO](
        implicitly,
        sourceCodeEoParser(2)
      )

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): java.util.List[OdinAnalysisErrorInterop] =
      delegate
        .analyzeSourceCode(eoRepr)
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

    private val delegate = analysis
      .EOOdinAnalyzer
      .impl[String, IO]

    implicit private val runtime: IORuntime = IORuntime.global

    @throws[Exception]
    override def analyze(
      eoRepr: String
    ): util.List[OdinAnalysisErrorInterop] =
      delegate
        .analyzeSourceCode(eoRepr)
        .map(fromOdinAnalysisError)
        .compile
        .toList
        .unsafeRunSync()
        .asJava

  }

}
