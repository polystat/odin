package org.polystat.odin.analysis

import com.github.tarao.nonempty.collection.NonEmpty
import monocle.Optional
import org.polystat.odin.core.ast.{EOBnd, EOBndExpr, EONamedBnd, EOObj}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import inlining.types._

package inlining {

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

  sealed trait BndPlaceholder
  case class MethodPlaceholder(name: EONamedBnd) extends BndPlaceholder
  case class ObjectPlaceholder(name: EONamedBnd) extends BndPlaceholder
  case class BndItself(bnd: EOBndExpr[EOExprOnly]) extends BndPlaceholder

  case class Object(
    name: EONamedBnd,
    methods: Map[EONamedBnd, MethodInfo],
    nestedObjects: Map[EONamedBnd, Object],
    bnds: Vector[BndPlaceholder],
    depth: BigInt,
  )

  case class MethodInfo(
    calls: Vector[Call],
    body: EOObj[EOExprOnly],
    depth: BigInt
  )

  case class Call(
    depth: BigInt,
    methodName: String,
    callSite: PathToCallSite,
    callLocation: PathToCall,
    args: CopyArgs
  )

}
