package org.polystat.odin.analysis.inlining

import cats.syntax.foldable._
import higherkindness.droste.data.Fix
import monocle.Iso
import Optics._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.analysis.inlining.types._

object LocateCalls {

  def hasNoReferencesToPhi(
    binds: Vector[EOBnd[Fix[EOExpr]]]
  ): Boolean = {

    def exprHelper(
      expr: Fix[EOExpr],
      upperBndName: Option[String]
    ): Option[String] = Fix.un(expr) match {
      case EOCopy(trg, args) =>
        exprHelper(trg, upperBndName)
          .orElse(
            args.value.foldMapK(bndHelper(upperBndName))
          )
      case EODot(trg, _) =>
        exprHelper(trg, upperBndName)
      case EOObj(_, _, bnds) => bnds.foldMapK(bndHelper(upperBndName))
      case EOSimpleAppWithLocator("@", _) =>
        upperBndName
      case _ =>
        None
    }

    def bndHelper(
      upperBndName: Option[String]
    )(bnd: EOBnd[Fix[EOExpr]]): Option[String] =
      bnd match {
        case EOAnonExpr(expr) => exprHelper(expr, upperBndName)
        case EOBndExpr(_, expr) => exprHelper(expr, upperBndName)
      }

    binds.flatMap {
      case EOAnonExpr(_) => None
      case EOBndExpr(bndName, expr) =>
        exprHelper(expr, Some(bndName.name.name))
    }.isEmpty
  }

  def hasPhiAttribute(bnds: Vector[EOBnd[EOExprOnly]]): Boolean =
    bnds.exists {
      case EOBndExpr(EODecoration, _) => true
      case _ => false
    }

  def hasSelfAsFirstParam(params: Vector[LazyName]): Boolean =
    params.headOption.exists(_.name == "self")

  def parseMethod(
    bnd: EOBndExpr[EOExprOnly],
    bndDepth: BigInt
  ): Option[MethodInfo] = {
    def findCalls(body: EOObj[EOExprOnly]): Vector[Call] = {
      def findCallsRec(
        subExpr: Fix[EOExpr],
        pathToCallSite: PathToCallSite,
        pathToCall: PathToCall,
        depth: BigInt,
      ): Vector[Call] = {
        Fix.un(subExpr) match {
          // the call was found
          case EOCopy(
                 Fix(EODot(Fix(EOSimpleAppWithLocator("self", locator)), name)),
                 args
               ) if locator == depth =>
            val firstArgIsValid: Boolean = args
              .headOption
              .exists {
                case EOAnonExpr(EOSimpleAppWithLocator("self", locator))
                     if locator == depth => true
                case _ => false
              }
            if (firstArgIsValid)
              Vector(
                Call(
                  depth = depth,
                  methodName = name,
                  callSite = pathToCallSite,
                  callLocation = pathToCall,
                  args = args
                )
              )
            else
              Vector()

          // the new callsite was found
          // pathToCall points to the new callsite
          // pathToCall is reset relative to this new callsite
          // depth is incremented
          case EOObj(_, _, bndAttrs) =>
            bndAttrs.zipWithIndex.flatMap { case (bnd, i) =>
              findCallsRec(
                subExpr = bnd.expr,
                pathToCallSite =
                  pathToCallSite.andThen(pathToCall).andThen(prisms.fixToEOObj),
                pathToCall = focusBndAttrAtIndex(i),
                depth = depth + 1
              )
            }

          // looking for calls in copy trg and args
          case EOCopy(trg, args) => findCallsRec(
              subExpr = trg,
              pathToCallSite = pathToCallSite,
              pathToCall = pathToCall
                .andThen(prisms.fixToEOCopy)
                .andThen(lenses.focusCopyTrg),
              depth = depth
            ) ++ args.zipWithIndex.flatMap { case (arg, i) =>
              findCallsRec(
                subExpr = arg.expr,
                pathToCallSite = pathToCallSite,
                pathToCall = pathToCall
                  .andThen(prisms.fixToEOCopy)
                  .andThen(focusCopyArgAtIndex(i)),
                depth = depth
              )
            }

          // looking for calls in EODot src
          case EODot(src, _) => findCallsRec(
              subExpr = src,
              pathToCallSite = pathToCallSite,
              pathToCall = pathToCall
                .andThen(prisms.fixToEODot)
                .andThen(lenses.focusDotSrc),
              depth = depth
            )

          // looking for calls in EOArray elements
          case EOArray(elems) => elems.zipWithIndex.flatMap { case (elem, i) =>
              findCallsRec(
                subExpr = elem.expr,
                pathToCallSite = pathToCallSite,
                pathToCall = pathToCall
                  .andThen(prisms.fixToEOArray)
                  .andThen(focusArrayElemAtIndex(i)),
                depth = depth
              )
            }

          // any other node can not contain calls
          case _ => Vector()
        }
      }

      body.bndAttrs.zipWithIndex.flatMap { case (bnd, i) =>
        findCallsRec(
          subExpr = bnd.expr,
          pathToCallSite = Iso.id[EOObj[EOExprOnly]],
          pathToCall = focusBndAttrAtIndex(i),
          depth = 0
        )
      }
    }

    Fix.un(bnd.expr) match {
      case obj @ EOObj(params, _, bndAttrs)
           if hasSelfAsFirstParam(params) &&
             hasPhiAttribute(bndAttrs) &&
             hasNoReferencesToPhi(bndAttrs) =>
        Some(MethodInfo(findCalls(obj), obj, bndDepth))
      case _ => None
    }
  }

}
