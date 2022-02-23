package org.polystat.odin.analysis.inlining

import com.github.tarao.nonempty.collection.NonEmpty
import monocle.Optional
import org.polystat.odin.core.ast.{EOBnd, EOObj}
import org.polystat.odin.core.ast.astparams.EOExprOnly

object types {

  type CopyArgs = NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]

  type PathToCallSite = Optional[
    EOObj[EOExprOnly], // method body
    EOObj[EOExprOnly], // call site object
  ]

  type PathToCall = Optional[
    EOObj[EOExprOnly], // call site
    EOExprOnly, // method call
  ]

}