package org.polystat.odin.interop.java

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.polystat.odin.interop.java.OdinAnalysisErrorInterop.fromOdinAnalysisError
import org.polystat.odin.analysis

import scala.jdk.CollectionConverters._

trait EOOdinAnalyzer {
  def analyzeSourceCode(code: String): java.util.List[OdinAnalysisErrorInterop]
}

object EOOdinAnalyzer {

  class EOOdinAnalyzerImpl() extends EOOdinAnalyzer {
    private val delegate = analysis.EOOdinAnalyzer.impl[IO]
    implicit private val runtime: IORuntime = IORuntime.global

    override def analyzeSourceCode(
      code: String
    ): java.util.List[OdinAnalysisErrorInterop] =
      delegate
        .analyzeSourceCode(code)
        .map(fromOdinAnalysisError)
        .compile
        .toList
        .unsafeRunSync()
        .asJava

  }

}
