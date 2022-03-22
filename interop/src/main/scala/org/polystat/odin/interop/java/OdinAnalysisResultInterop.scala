package org.polystat.odin.interop.java

import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult._

class OdinAnalysisResultInterop(
  val analysisName: java.lang.String,
  val detectedDefects: java.util.Optional[java.lang.String],
  val analyzerFailure: java.util.Optional[java.lang.String],
) {

  override def toString(): String =
    s"""OdinAnalysisErrorInterop(
       |  analysisName = "$analysisName",
       |  detectedDefects = $detectedDefects,
       |  analyzerFailure = $analyzerFailure,
       |)""".stripMargin

}

object OdinAnalysisResultInterop {

  private[java] def fromOdinAnalysisResult(
    oar: OdinAnalysisResult
  ): List[OdinAnalysisResultInterop] = {
    oar match {
      case Ok(analysisName) =>
        List(
          new OdinAnalysisResultInterop(
            analysisName,
            java.util.Optional.empty,
            java.util.Optional.empty,
          )
        )
      case DefectDetected(analysisName, message) =>
        List(
          new OdinAnalysisResultInterop(
            analysisName,
            java.util.Optional.of(message),
            java.util.Optional.empty,
          )
        )
      case AnalyzerFailure(analysisName, reason) =>
        List(
          new OdinAnalysisResultInterop(
            analysisName,
            java.util.Optional.empty,
            java.util.Optional.of(reason),
          )
        )
    }
  }

}
