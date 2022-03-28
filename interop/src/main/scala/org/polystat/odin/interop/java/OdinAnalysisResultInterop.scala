package org.polystat.odin.interop.java

import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult._

class OdinAnalysisResultInterop(
  val ruleId: java.lang.String,
  val detectedDefect: java.util.Optional[java.lang.String],
  val analyzerFailure: java.util.Optional[java.lang.Throwable],
) {

  override def toString(): String =
    s"""OdinAnalysisErrorInterop(
       |  analysisName = "$ruleId",
       |  detectedDefects = $detectedDefect,
       |  analyzerFailure = $analyzerFailure,
       |)""".stripMargin

}

object OdinAnalysisResultInterop {

  private[java] def fromOdinAnalysisResult(
    oar: OdinAnalysisResult
  ): List[OdinAnalysisResultInterop] = {
    oar match {
      case Ok(rule) =>
        List(
          new OdinAnalysisResultInterop(
            rule,
            java.util.Optional.empty,
            java.util.Optional.empty,
          )
        )
      case DefectDetected(rule, message) =>
        List(
          new OdinAnalysisResultInterop(
            rule,
            java.util.Optional.of(message),
            java.util.Optional.empty,
          )
        )
      case AnalyzerFailure(rule, reason) =>
        List(
          new OdinAnalysisResultInterop(
            rule,
            java.util.Optional.empty,
            java.util.Optional.of(reason),
          )
        )
    }
  }

}
