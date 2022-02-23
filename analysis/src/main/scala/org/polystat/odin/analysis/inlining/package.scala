package org.polystat.odin.analysis

import org.polystat.odin.core.ast.{EOBndExpr, EONamedBnd, EOObj}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import inlining.types._

package inlining {

  sealed trait BndPlaceholder
  final case class MethodPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class ObjectPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class BndItself(bnd: EOBndExpr[EOExprOnly]) extends BndPlaceholder

  final case class Object(
    name: EONamedBnd,
    methods: Map[EONamedBnd, MethodInfo],
    nestedObjects: Map[EONamedBnd, Object],
    bnds: Vector[BndPlaceholder],
    depth: BigInt,
  )

//  case class Assert()

  final case class MethodInfo(
    calls: Vector[Call],
    body: EOObj[EOExprOnly],
    depth: BigInt,
//    asserts: Vector[Assert]
  )

  final case class Call(
    depth: BigInt,
    methodName: String,
    callSite: PathToCallSite,
    callLocation: PathToCall,
    args: CopyArgs
  )

}
