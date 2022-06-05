package org.polystat.odin.analysis.utils.inlining

import cats.data.{NonEmptyList => Nel}
import cats.syntax.foldable._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.ObjectName
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import LocateCalls._

object LocateMethods {

  def isNotObj(expr: EOExprOnly): Boolean =
    Fix.un(expr) match {
      case EOObj(_, _, _) => false
      case _ => true
    }

  def parseParentName(
    bnd: EOBnd[EOExprOnly],
    bnds: Vector[EOBndExpr[Fix[EOExpr]]]
  ): Option[ObjectNameWithLocator] = {

    def parseObjectName(
      expr: EOExprOnly,
    ): Option[ObjectNameWithLocator] = {
      Fix.un(expr) match {
        case EOSimpleAppWithLocator(name, locator)
             if locator == 0 && isNotObj(bnds.find(_.bndName.name.name == name).get.expr) =>
          bnds.collectFirst {
            case EOBndExpr(bndName, expr) if name == bndName.name.name =>
              parseObjectName(expr)
          }.flatten
        case EOSimpleAppWithLocator(name, locator) =>
          Some(
            ObjectNameWithLocator(locator, ObjectName(Nel.one(name)))
          )
        case EODot(trg, dotName) =>
          parseObjectName(trg).map(parent =>
            parent.copy(name =
              ObjectName(parent.name.names.concatNel(Nel.one(dotName)))
            )
          )
        // TODO: add a proper depth check (disambiguate `seq`)
        case EOCopy(EOSimpleAppWithLocator("seq", _), args) =>
          args.last match {
            case EOAnonExpr(expr) => parseObjectName(expr)
            case _ => None
          }
        case _ => None
      }
    }

    bnd match {
      case EOBndExpr(EODecoration, expr) => parseObjectName(expr)
      case _ => None
    }
  }

  private case class BndInfo(
    parentName: Option[ObjectNameWithLocator],
    nestedObjs: Map[EONamedBnd, ObjectTree[ObjectInfo[ParentName, MethodInfo]]],
    methods: Map[EONamedBnd, MethodInfo],
    otherBnds: Vector[BndPlaceholder],
  )

  def parseObject(
    obj: EOBnd[EOExprOnly],
    objDepth: BigInt,
    containingNames: List[String],
  ): Option[ObjectTree[ObjectInfo[ParentName, MethodInfo]]] = {

    obj match {
      case EOBndExpr(bndName, Fix(EOObj(Vector(), None, bnds))) =>
        val fqn = Nel.ofInitLast(containingNames, bndName.name.name)
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
                parseObject(next, objDepth + 1, fqn.toList)
                  .map(o =>
                    acc.copy(
                      nestedObjs = objects.updated(next.bndName, o),
                      otherBnds = otherBnds.appended(
                        ObjectPlaceholder(next.bndName)
                      )
                    )
                  ),
                parseParentName(next, bnds)
                  .map(p =>
                    acc.copy(
                      parentName = Some(p),
                      otherBnds =
                        otherBnds.appended(ParentPlaceholder(next.expr))
                    )
                  ),
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
              fqn = ObjectName(fqn),
            ),
            children = objects,
          )
        )

      case _ => None
    }
  }

}
