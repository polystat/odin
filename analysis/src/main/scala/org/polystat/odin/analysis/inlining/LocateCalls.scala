package org.polystat.odin.analysis.inlining

import cats.data.NonEmptyList
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import monocle.{Iso, Optional}
import Optics._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

object LocateCalls {

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

  case class Object(
    name: EONamedBnd,
    methods: Map[String, MethodInfo],
    bnds: Vector[Either[MethodPlaceholder, EOBndExpr[EOExprOnly]]],
  )

  case class MethodPlaceholder(methodName: String)

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

  def createMethod(m: EOBndExpr[EOExprOnly]): Option[MethodInfo] = {
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
    Fix.un(m.expr) match {
      case obj @ EOObj(params, _, _)
           if params.headOption.exists(_.name == "self") =>
        Some(MethodInfo(findCalls(obj), obj))
      case _ => None
    }
  }

}
