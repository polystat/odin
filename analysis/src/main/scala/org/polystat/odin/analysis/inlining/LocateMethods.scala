package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

object LocateMethods {

  def parseObject(
    obj: EOBnd[EOExprOnly],
    objDepth: BigInt
  ): Option[Object] = {
    type BndInfo = (
      Map[EONamedBnd, Object],
      Map[EONamedBnd, MethodInfo],
      Vector[BndPlaceholder],
    )
    obj match {
      case EOBndExpr(bndName, Fix(EOObj(Vector(), None, bnds))) =>
        val (objects, methods, otherBnds) =
          bnds.foldLeft[BndInfo]((Map(), Map(), Vector())) {
            case ((objects, methods, otherBnds), next) =>
              LocateCalls.parseMethod(next, objDepth + 1) match {
                case Some(method) => (
                    objects,
                    methods.updated(next.bndName, method),
                    otherBnds.appended(
                      MethodPlaceholder(next.bndName)
                    )
                  )
                case None => parseObject(next, objDepth + 1) match {
                    case Some(obj) => (
                        objects.updated(next.bndName, obj),
                        methods,
                        otherBnds.appended(
                          ObjectPlaceholder(next.bndName)
                        )
                      )
                    case None => (
                        objects,
                        methods,
                        otherBnds.appended(
                          BndItself(next)
                        )
                      )
                  }
              }
          }

        Some(
          Object(
            name = bndName,
            methods = methods,
            bnds = otherBnds,
            nestedObjects = objects,
            depth = objDepth
          )
        )

      case _ => None
    }
  }

}
