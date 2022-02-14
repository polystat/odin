package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.analysis.inlining.types._

object LocateMethods {

  def parseIfObject(
    obj: EOBnd[EOExprOnly],
    objDepth: BigInt
  ): Option[Object] = {
    type BndInfo = (
      Map[EONamedBnd, MethodInfo],
      Vector[Either[MethodPlaceholder, EOBndExpr[EOExprOnly]]]
    )
    obj match {
      case EOBndExpr(bndName, Fix(EOObj(Vector(), None, bnds))) =>
        val (methods, otherBnds) = bnds.foldLeft[BndInfo]((Map(), Vector())) {
          case ((methods, otherBnds), next) =>
            LocateCalls.createMethod(next, objDepth) match {
              case Some(value) => (
                  methods.updated(next.bndName, value),
                  otherBnds.appended(
                    Left(next.bndName)
                  )
                )
              case None => (methods, Vector(Right(next)))
            }
        }

        Some(
          Object(
            name = bndName,
            methods = methods,
            bnds = otherBnds
          )
        )

      case _ => None
    }
  }

}
