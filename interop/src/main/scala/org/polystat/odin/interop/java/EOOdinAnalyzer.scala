package org.polystat.odin.interop.java

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.polystat.odin.interop.java.OdinAnalysisErrorInterop.fromOdinAnalysisError
import org.polystat.odin.analysis
import fs2.interop.reactivestreams.StreamOps
import org.reactivestreams.Publisher

trait EOOdinAnalyzer {
  def analyzeSourceCode(code: String): Publisher[OdinAnalysisErrorInterop]
}

object EOOdinAnalyzer {
  class EOOdinAnalyzerImpl() extends EOOdinAnalyzer {
    private val delegate = analysis.EOOdinAnalyzer.impl[IO]
    implicit private val runtime: IORuntime = IORuntime.global

    override def analyzeSourceCode(code: String): Publisher[OdinAnalysisErrorInterop] =
      delegate.analyzeSourceCode(code)
        .map(fromOdinAnalysisError)
        .toUnicastPublisher
        .use(IO.pure)
        .unsafeRunSync()
  }
}
