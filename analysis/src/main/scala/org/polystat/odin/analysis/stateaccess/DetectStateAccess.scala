package org.polystat.odin.analysis.stateaccess

import cats.data.EitherNel
import cats.syntax.all._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.ObjectName
import org.polystat.odin.analysis.utils.Abstract
import org.polystat.odin.analysis.utils.inlining._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import scala.annotation.tailrec

object DetectStateAccess {

  type ObjInfo = ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]

  case class State(
    statePath: List[String],
    state: EONamedBnd
  )

  case class StateChange(
    method: EONamedBnd,
    statePath: List[String],
    state: EONamedBnd,
  )

  def isJ2EOPrimitive(source: EOExprOnly): Boolean = {
    val j2eoPrimitives = Seq(
      "prim__boolean",
      "prim__byte",
      "prim__char",
      "prim__double",
      "prim__float",
      "prim__int",
      "prim__num",
      "prim__short",
      "prin__long",
    )

    Fix.un(source) match {
      case EOSimpleAppWithLocator(name, _) if j2eoPrimitives.contains(name) =>
        true
      case _ => false
    }
  }

  def collectLocalStates(
    obj: Inliner.CompleteObjectTree,
    existingStates: Map[ObjectName, Vector[State]] = Map.empty
  )(implicit originalObjInfo: ObjInfo): Map[ObjectName, Vector[State]] = {
    val statePath = obj
      .info
      .fqn
      .names
      .toList
      .drop(originalObjInfo.depth.toInt + 1)

    def notAlreadyPresent(bndName: EONamedBnd): Boolean = !existingStates
      .values
      .toList
      .flatten
      .contains(
        State(
          statePath,
          bndName
        )
      )

    val parentInfo = obj.info
    val currentLvlStates = parentInfo
      .bnds
      .collect {
        case BndItself(
               EOBndExpr(
                 bndName,
                 // TODO: add a depth check here?
                 EOSimpleAppWithLocator("memory" | "cage", _)
               )
             ) if notAlreadyPresent(bndName) =>
          State(
            statePath,
            bndName
          )

        case BndItself(
               EOBndExpr(
                 bndName,
                 Fix(
                   EOCopy(
                     Fix(EODot(source, _)),
                     _,
                   )
                 )
               )
             ) if notAlreadyPresent(bndName) && isJ2EOPrimitive(source) =>
          State(
            statePath,
            bndName
          )
      }
    val currentLvlMap = Map(originalObjInfo.fqn -> currentLvlStates)
    val nestedStates = obj
      .children
      .flatMap(c =>
        collectLocalStates(
          c._2,
          existingStates |+| currentLvlMap
        )
      )

    currentLvlMap |+| nestedStates
  }

  def accumulateParentState(tree: Map[EONamedBnd, Inliner.CompleteObjectTree])(
    childObject: ObjInfo,
    existingStates: Map[ObjectName, Vector[State]],
    previousParents: Vector[ObjectName] = Vector(),
  ): EitherNel[String, Map[ObjectName, Vector[State]]] = {
    childObject.parentInfo match {
      case Some(parentLink) =>
        val parentObj = parentLink.linkToParent.getOption(tree).get
        val parentInfo = parentObj.info
        val localStates = collectLocalStates(
          parentObj,
          existingStates
        )(parentObj.info)

        if (previousParents.contains(parentInfo.fqn)) {
          val prettyChain =
            previousParents
              .appended(parentInfo.fqn)
              .map(_.show)
              .mkString(" -> ")
          s"There is a cycle in the decoration chain: $prettyChain".leftNel
        } else
          accumulateParentState(tree)(
            parentInfo,
            existingStates |+| localStates,
            previousParents.appended(parentInfo.fqn)
          ).map(res => localStates |+| res)

      case None => Right(Map.empty)
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
        case innerDot @ EODot(_, _) => hasSelfAsSource(innerDot, depth)
        case _ => false
      }
    }

    def buildDotChain(dot: EODot[EOExprOnly]): List[String] =
      Fix.un(dot.src) match {
        // TODO: add depth check???
        case EOSimpleAppWithLocator(argName, _) if argName == selfArgName =>
          List()
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

      List(StateChange(method._1, containerChain, stateName))
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
  )(
    obj: (EONamedBnd, Inliner.CompleteObjectTree)
  ): EitherNel[String, List[String]] = {
    val localStates = collectLocalStates(obj._2)(obj._2.info)
    val availableParentStates =
      accumulateParentState(tree)(obj._2.info, localStates)
    val accessedStates =
      obj._2.info.methods.flatMap { case method @ (_, methodInfo) =>
        getAccessedStates(methodInfo.selfArgName)(method)
      }
    availableParentStates
      .map(_.toList)
      .map(baseStatePairs =>
        for {
          baseClass -> states <- baseStatePairs
          State(statePath, availableState) <- states
          StateChange(targetMethod, accessedStatePath, changedState) <-
            accessedStates
        } yield
          if (
            availableState == changedState && statePath == accessedStatePath
          ) {
            val objName = obj._2.info.fqn.names.toList.mkString(".")
            val stateName =
              statePath.appended(availableState.name.name).mkString(".")
            val method = targetMethod.name.name

            List(
              f"Method '$method' of object '$objName' directly accesses state '$stateName' of base class '${baseClass.show}'"
            )
          } else List()
      )
      .map(_.flatten)
  }

  def analyze[F[_]](
    originalTree: Map[EONamedBnd, Inliner.CompleteObjectTree]
  ): EitherNel[String, List[String]] = {
    def helper(
      tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
    ): EitherNel[String, List[String]] =
      tree
        .filter(_._2.info.parentInfo.nonEmpty)
        .toList
        .flatTraverse(detectStateAccesses(originalTree))
        .map(_.toList)

    def recurse(
      tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
    ): EitherNel[String, List[String]] = for {
      currentRes <- helper(tree)
      children = tree.values.map(_.children)
      childRes <- children.toList.flatTraverse(recurse)
    } yield currentRes ++ childRes

    recurse(originalTree)
  }

}
