package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import LocateCalls._
import cats.syntax.foldable._

object LocateMethods {

  def parseParentName(
    bnd: EOBnd[EOExprOnly]
  ): Option[ObjectNameWithLocator] = {

    def parseObjectName(
      expr: EOExprOnly,
      parent: Option[ObjectName]
    ): Option[ObjectNameWithLocator] = {
      Fix.un(expr) match {
        case EODot(Fix(EOSimpleAppWithLocator(parentName, locator)), name) =>
          Some(
            ObjectNameWithLocator(
              locator,
              ObjectName(Some(ObjectName(None, parentName)), name)
            )
          )
        case EODot(trg, dotName) =>
          parseObjectName(trg, Some(ObjectName(parent, dotName))).map(obj =>
            obj.copy(name = ObjectName(Some(obj.name), dotName))
          )
        case _ => None
      }
    }

    bnd match {
      case EOBndExpr(EODecoration, expr) => parseObjectName(expr, None)
      case _ => None
    }
  }

  private case class BndInfo(
    parentName: Option[ObjectNameWithLocator],
    nestedObjs: Map[EONamedBnd, ObjectTree[ParentName, MethodInfo, ObjectInfo]],
    methods: Map[EONamedBnd, MethodInfo],
    otherBnds: Vector[BndPlaceholder],
  )

  def parseObject(
    obj: EOBnd[EOExprOnly],
    objDepth: BigInt
  ): Option[ObjectTree[ParentName, MethodInfo, ObjectInfo]] = {

    obj match {
      case EOBndExpr(bndName, Fix(EOObj(Vector(), None, bnds))) =>
        val BndInfo(parentName, objects, methods, otherBnds) =
          bnds.foldLeft[BndInfo](BndInfo(None, Map(), Map(), Vector())) {
            case (
                   acc @ BndInfo(_, objects, methods, otherBnds),
                   next
                 ) =>
              List(
                parseMethod(next, objDepth + 1)
                  .map(m =>
                    acc.copy(
                      methods = methods.updated(next.bndName, m),
                      otherBnds = otherBnds.appended(
                        MethodPlaceholder(next.bndName)
                      )
                    )
                  ),
                parseObject(next, objDepth + 1)
                  .map(o =>
                    acc.copy(
                      nestedObjs = objects.updated(next.bndName, o),
                      otherBnds = otherBnds.appended(
                        ObjectPlaceholder(next.bndName)
                      )
                    )
                  ),
                parseParentName(next)
                  .map(p => acc.copy(parentName = Some(p))),
              ).foldK
                .getOrElse(
                  acc.copy(
                    otherBnds = otherBnds.appended(BndItself(next))
                  )
                )
          }

        Some(
          ObjectTree(
            info = ObjectInfo(
              name = bndName,
              parentInfo = parentName.map(ParentName.apply),
              methods = methods,
              bnds = otherBnds,
              depth = objDepth,
            ),
            children = objects,
          )
        )

      case _ => None
    }
  }

}
