package org.polystat.odin.analysis.utils.inlining

import monocle.Optional
import org.polystat.odin.core.ast.EOObj
import org.polystat.odin.core.ast.astparams.EOExprOnly

object types {

  type PathToCallSite = Optional[
    EOObj[EOExprOnly], // method body
    EOObj[EOExprOnly], // call site object
  ]

  type PathToCall = Optional[
    EOObj[EOExprOnly], // call site
    EOExprOnly, // method call
  ]

}
