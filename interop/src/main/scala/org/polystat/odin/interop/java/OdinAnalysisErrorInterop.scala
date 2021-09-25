package org.polystat.odin.interop.java

import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError

case class OdinAnalysisErrorInterop(message: String)

object OdinAnalysisErrorInterop {

  implicit def fromOdinAnalysisError(
    e: OdinAnalysisError
  ): OdinAnalysisErrorInterop = OdinAnalysisErrorInterop(e.value)

}
