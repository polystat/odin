package org.polystat.odin.interop.java

import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisResult._
import scala.jdk.CollectionConverters._

class OdinAnalysisResultInterop(
  val analysisName: java.lang.String,
  val detectedDefects: java.util.List[java.lang.String],
  val analyzerFailure: java.util.Optional[java.lang.String],
) {

  override def toString(): String =
    s"""OdinAnalysisErrorInterop(
       |  analysisName = "$analysisName",
       |  detectedDefects = ${detectedDefects
        .asScala
        .toList
        .map(_.mkString("\"", "", "\""))},
       |  analyzerFailure = $analyzerFailure,
       |)""".stripMargin

}

object OdinAnalysisResultInterop {

  private[java] def fromOdinAnalysisResult(
    oar: OdinAnalysisResult
  ): List[OdinAnalysisResultInterop] = {
    oar match {
      case Ok(_) =>
        List.empty
      case DefectDetected(analysisName, messages) =>
        List(
          new OdinAnalysisResultInterop(
            analysisName,
            messages.asJava,
            java.util.Optional.empty,
          )
        )
      case AnalyzerFailure(analysisName, reason) =>
        List(
          new OdinAnalysisResultInterop(
            analysisName,
            List.empty.asJava,
            java.util.Optional.of(reason),
          )
        )
    }
  }

}
