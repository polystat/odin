package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import monocle.Iso
import Optics._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.analysis.inlining.types._

object LocateCalls {

  def hasNoReferencesToPhi(binds: Vector[EOBnd[Fix[EOExpr]]]): Boolean = {
    def recurse(bnd: EOBnd[EOExprOnly]): Boolean = {
      Fix.un(bnd.expr) match {
        case EOSimpleAppWithLocator("@", _) => false
        case EOObj(_, _, bndAttrs) => bndAttrs.map(recurse).forall(identity)
        case EOCopy(trg, args) =>
          recurse(EOAnonExpr(trg)) && args.map(recurse).forall(identity)
        case EODot(trg, _) => recurse(EOAnonExpr(trg))
        case EOArray(elems) => elems.map(recurse).forall(identity)
        case _ => true
      }
    }
    binds.map(recurse).forall(identity)
  }

  def hasPhiAttribute(bnds: Vector[EOBnd[EOExprOnly]]): Boolean =
    bnds.exists {
      case EOBndExpr(EODecoration, _) => true
      case _ => false
    }

  def hasSelfAsFirstParam(params: Vector[LazyName]): Boolean =
    params.headOption.exists(_.name == "self")

  def parseMethod(
    methodBnd: EOBndExpr[EOExprOnly],
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
            bndAttrs.flatMap { bnd =>
              findCallsRec(
                subExpr = bnd.expr,
                pathToCallSite =
                  pathToCallSite.andThen(pathToCall).andThen(prisms.fixToEOObj),
                pathToCall = optionals.focusBndAttrWithName(bnd.bndName),
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
                  .andThen(optionals.focusCopyArgAtIndex(i)),
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
                  .andThen(optionals.focusArrayElemAtIndex(i)),
                depth = depth
              )
            }

          // any other node can not contain calls
          case _ => Vector()
        }
      }

      body.bndAttrs.flatMap { bnd =>
        findCallsRec(
          subExpr = bnd.expr,
          pathToCallSite = Iso.id[EOObj[EOExprOnly]],
          pathToCall = optionals.focusBndAttrWithName(bnd.bndName),
          depth = 0
        )
      }
    }

    Fix.un(methodBnd.expr) match {
      case obj @ EOObj(params, _, bndAttrs)
           if hasSelfAsFirstParam(params) &&
             hasPhiAttribute(bndAttrs) &&
             hasNoReferencesToPhi(bndAttrs) =>
        Some(MethodInfo(findCalls(obj), obj, bndDepth))
      case _ => None
    }
  }

}
