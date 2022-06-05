package org.polystat.odin.analysis.stateaccess

import cats.data.EitherNel
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.utils.Abstract
import org.polystat.odin.analysis.utils.inlining._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import scala.annotation.tailrec

object DetectStateAccess {

  type ObjInfo = ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]

  case class State(
    container: ObjInfo,
    statePath: List[String],
    states: Vector[EONamedBnd]
  )

  case class StateChange(
    method: EONamedBnd,
    state: EONamedBnd,
    statePath: List[String]
  )

  def collectNestedStates(mainParent: ObjInfo)(
    subTree: Inliner.CompleteObjectTree,
    depth: Int
  ): Vector[State] = {
    val currentLvlStateNames = subTree
      .info
      .bnds
      .collect {
        case BndItself(
               EOBndExpr(
                 bndName,
                 EOSimpleAppWithLocator("memory" | "cage", _)
               )
             ) => bndName
      }

    Vector(
      State(
        mainParent,
        subTree.info.fqn.names.toList.drop(depth),
        currentLvlStateNames
      )
    ) ++
      subTree
        .children
        .flatMap(t => collectNestedStates(mainParent)(t._2, depth))
  }

  def accumulateParentState(tree: Map[EONamedBnd, Inliner.CompleteObjectTree])(
    currentParentLink: Option[ParentInfo[MethodInfo, ObjectInfo]],
    existingStates: Vector[EONamedBnd] = Vector()
  ): Vector[State] = {
    currentParentLink match {
      case Some(parentLink) =>
        val parentObj = parentLink.linkToParent.getOption(tree).get
        val currentLvlStateNames = parentObj
          .info
          .bnds
          .collect {
            case BndItself(
                   EOBndExpr(
                     bndName,
                     EOSimpleAppWithLocator("memory" | "cage", _)
                   )
                 ) if !existingStates.contains(bndName) =>
              bndName
          }
        val currentLvlState =
          State(parentObj.info, List(), currentLvlStateNames)
        val nestedStates = parentObj
          .children
          .flatMap(c =>
            collectNestedStates(parentObj.info)(
              c._2,
              parentObj.info.depth.toInt + 1
            )
          )
          .toVector

        Vector(currentLvlState) ++ nestedStates ++
          accumulateParentState(tree)(
            parentObj.info.parentInfo,
            existingStates ++ currentLvlStateNames
          )

      case None => Vector()
    }
  }

  def getAccessedStates(selfArgName: String)(
    method: (EONamedBnd, MethodInfo)
  ): List[StateChange] = {
    @tailrec
    def hasSelfAsSource(dot: EODot[EOExprOnly], depth: BigInt): Boolean = {
      Fix.un(dot.src) match {
        // TODO: add a proper depth check
        case EOSimpleAppWithLocator(dotSrc, x)
             if x == depth && dotSrc == selfArgName => true
        case innerDot @ EODot(_, _) => hasSelfAsSource(innerDot)
        case _ => false
      }
    }

    def buildDotChain(dot: EODot[EOExprOnly]): List[String] =
      Fix.un(dot.src) match {
        // TODO: add depth check???
        case EOSimpleAppWithLocator(argName, _)
             if argName == selfArgName => List()
        case innerDot @ EODot(_, _) =>
          buildDotChain(innerDot).appended(innerDot.name)
        case _ => List()
      }

    val binds = method._2.body.bndAttrs

    def processDot(
      innerDot: EODot[Fix[EOExpr]],
      state: String
    ): List[StateChange] = {
      val stateName = EOAnyNameBnd(LazyName(state))
      val containerChain = buildDotChain(innerDot)

      List(StateChange(method._1, stateName, containerChain))
    }

    Abstract.foldAst[List[StateChange]](binds, 0) {
      case (
             EOCopy(Fix(dot @ EODot(Fix(innerDot @ EODot(_, state)), _)), _),
             depth
           ) if hasSelfAsSource(dot, depth) =>
        processDot(innerDot, state)

      case (dot @ EODot(_, state), depth) if hasSelfAsSource(dot, depth) =>
        processDot(dot, state)
    }
  }

  def detectStateAccesses(
    tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
  )(obj: (EONamedBnd, Inliner.CompleteObjectTree)): List[String] = {
    val availableParentStates =
      accumulateParentState(tree)(obj._2.info.parentInfo)
    val accessedStates =
      obj._2.info.methods.flatMap { case method @ (_, methodInfo) =>
        getAccessedStates(methodInfo.selfArgName)(method)
      }
    val results =
      for {
        StateChange(targetMethod, state, accessedStatePath) <- accessedStates
        State(baseClass, statePath, changedStates) <- availableParentStates
      } yield
        if (changedStates.contains(state) && statePath == accessedStatePath) {
          val objName = obj._2.info.fqn.names.toList.mkString(".")
          val stateName = statePath.appended(state.name.name).mkString(".")
          val method = targetMethod.name.name
          val container = baseClass.fqn.show

          List(
            f"Method '$method' of object '$objName' directly accesses state '$stateName' of base class '$container'"
          )
        } else List()

    results.toList.flatten
  }

  def analyze[F[_]](
    originalTree: Map[EONamedBnd, Inliner.CompleteObjectTree]
  ): EitherNel[String, List[String]] = {
    def helper(
      tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
    ): List[String] =
      tree
        .filter(_._2.info.parentInfo.nonEmpty)
        .flatMap(detectStateAccesses(originalTree))
        .toList

    def recurse(
      tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
    ): List[String] = {
      val currentRes = helper(tree)
      val children = tree.values.map(_.children)

      currentRes ++ children.flatMap(recurse)
    }

    Right(recurse(originalTree))
  }

}
