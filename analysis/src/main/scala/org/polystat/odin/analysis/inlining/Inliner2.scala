package org.polystat.odin.analysis.inlining

import cats.MonadError
import cats.data.NonEmptyList
import cats.syntax.functor._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import cats.data.NonEmptyVector
import monocle.{Iso, Lens, Optional, Prism}
import Optics._

object Inliner2 {

  type Errors = NonEmptyList[String]

  case class Object(
    name: EONamedBnd,
    methods: Map[String, MethodInfo],
    bnds: Vector[Either[MethodPlaceholder, EOBndExpr[EOExprOnly]]],
  )

  def createMethod(m: EOBndExpr[EOExprOnly]): Option[MethodInfo] = {
    type PathToCallSite = Optional[
      EOObj[EOExprOnly], // method body (EOObj)
      EOExprOnly, // call site object (EOObj)
    ]
    type PathToCall = Optional[
      EOObj[EOExprOnly], // method body (EOObj)
      EOExprOnly, // method call (any EOExpr)
    ]
    type CallSitePathLink = Optional[EOExprOnly, EOObj[EOExprOnly]]

    val eoObjIdentityLense: Lens[EOObj[EOExprOnly], EOExprOnly] =
      Lens[EOObj[EOExprOnly], EOExprOnly](Fix(_))(_ => obj => obj)


    def findCalls(body: EOObj[EOExprOnly]): Vector[Call] = {

      def findCallsRec(
        subExpr: Fix[EOExpr],
        pathToCallSite: PathToCallSite,
        pathToCall: PathToCall,
        depth: BigInt,
      ): Vector[Call] = {

        Fix.un(subExpr) match {
          case EOObj(_, _, bndAttrs) =>
            bndAttrs.zipWithIndex.flatMap { case (bnd, i) =>
              findCallsRec(
                bnd.expr,
                pathToCallSite = pathToCall,
                pathToCall = pathToCall
                  .andThen(prismFromFixToEOObj)
                  .andThen(focusFromObjToItsBndAttrAtIndex(i)),
                depth
              )
            }
          case EOCopy(trg, args) => ???
          case EODot(trg, _) => ???
          case EOArray(elems) => ???
          case _ => Vector()
        }

      }

      body.bndAttrs.zipWithIndex.flatMap { case (bnd, i) =>
        findCallsRec(
          subExpr = bnd.expr,
          pathToCallSite = eoObjIdentityLense,
          pathToCall = focusFromObjToItsBndAttrAtIndex(i),
          depth = 0
        )
      }
    }

    Fix.un(m.expr) match {
      case obj @ EOObj(Vector(LazyName("self")) :+ _, _, _) =>
        Some(MethodInfo(findCalls(Fix(obj)), obj))
      case _ => None
    }

  }

  def inlineMethod[F[_]: MonadError[*, Errors]](obj: Object)(
    method: MethodPlaceholder
  ): F[EOBndExpr[EOExprOnly]] = ???

  def inlineMethods[F[_]](
    obj: Object
  )(implicit F: MonadError[F, Errors]): F[EOBndExpr[EOExprOnly]] = {
    obj
      .bnds
      .traverse {
        case Left(method) => inlineMethod(obj)(method)
        case Right(otherBnd) => F.pure(otherBnd)
      }
      .map(bnds =>
        EOBndExpr(
          bndName = obj.name,
          expr = Fix(EOObj(Vector(), None, bndAttrs = bnds))
        )
      )

  }

  case class MethodPlaceholder(methodName: String)

  case class MethodInfo(
    calls: Vector[Call],
    body: EOObj[EOExprOnly],
  )

  case class Call(
    depth: BigInt,
    methodName: String,
    callSite: Optional[EOObj[EOExprOnly], EOObj[EOExprOnly]],
    callLocation: Optional[EOObj[EOExprOnly], EOCopy[EOExprOnly]],
    args: NonEmptyVector[EOBnd[EOExprOnly]]
  )

}
