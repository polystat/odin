package org.polystat.odin.analysis

import cats.data.NonEmptyList
import com.github.tarao.nonempty.collection.NonEmpty
import monocle.Optional
import org.polystat.odin.core.ast.{EOBnd, EOBndExpr, EONamedBnd, EOObj}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import inlining.types._

package inlining {

  object types {
    type Errors = NonEmptyList[String]
    type CopyArgs = NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]

    type PathToCallSite = Optional[
      EOObj[EOExprOnly], // method body
      EOObj[EOExprOnly], // call site object
    ]

    type PathToCall = Optional[
      EOObj[EOExprOnly], // call site
      EOExprOnly, // method call
    ]

    type MethodPlaceholder = EONamedBnd

  }

  case class Object(
    name: EONamedBnd,
    methods: Map[EONamedBnd, MethodInfo],
    bnds: Vector[Either[MethodPlaceholder, EOBndExpr[EOExprOnly]]],
  )

  case class MethodInfo(
    calls: Vector[Call],
    body: EOObj[EOExprOnly],
  )

  case class Call(
    depth: BigInt,
    methodName: String,
    callSite: PathToCallSite,
    callLocation: PathToCall,
    args: CopyArgs
  )

}
