package org.polystat.odin.analysis.stateaccess

import cats.data.EitherNel
import cats.syntax.either._
import org.polystat.odin.analysis.inlining._
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.eo.Parser

object DetectStateAccess {

  type ObjInfo = ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]
  case class State(container: EONamedBnd, states: Vector[EONamedBnd])
  case class StateChange(method: EONamedBnd, state: EONamedBnd)

  def accumulateParentState(tree: Map[EONamedBnd, Inliner.CompleteObjectTree])(
    currentParentLink: Option[ParentInfo[MethodInfo, ObjectInfo]],
    existingState: Vector[EONamedBnd] = Vector()
  ): List[State] = {
    currentParentLink match {
      case Some(parentLink) =>
        val parentObj = parentLink.linkToParent.getOption(tree).get
        val currentStates = parentObj
          .info
          .bnds
          .collect {
            case BndItself(
                   EOBndExpr(
                     bndName,
                     EOSimpleAppWithLocator("memory" | "cage", _)
                   )
                 ) if !existingState.contains(bndName) =>
              bndName
          }

        List(State(container = parentObj.info.name, states = currentStates)) ++
          accumulateParentState(tree)(
            parentObj.info.parentInfo,
            existingState ++ currentStates
          )

      case None => List()
    }
  }

  def getAccessedStates(method: (EONamedBnd, MethodInfo)): List[StateChange] = {
    val binds = method._2.body.bndAttrs

    Abstract.foldAst[List[StateChange]](binds) {
      case EODot(EOSimpleAppWithLocator("self", x), state) if x == 0 =>
        List(StateChange(method._1, EOAnyNameBnd(LazyName(state))))
    }
  }

  def detectStateAccesses(
    tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
  )(obj: (EONamedBnd, Inliner.CompleteObjectTree)): List[String] = {
    val availableParentStates =
      accumulateParentState(tree)(obj._2.info.parentInfo)
    val accessedStates = obj._2.info.methods.flatMap(getAccessedStates)
    val results =
      for {
        StateChange(changeSource, state) <- accessedStates
        State(stateContainer, changedStates) <- availableParentStates
      } yield
        if (changedStates.contains(state)) {
          val objName = obj._2.info.fqn.names.toList.mkString(".")
          val stateName = state.name.name
          val method = changeSource.name.name
          val container = stateContainer.name.name

          List(
            f"Method '${method}' of object '${objName}' directly accesses state '${stateName}' of base class '${container}'"
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
        // Has a parent
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

  def main(args: Array[String]): Unit = {
    val code = """
                 |[] > super
                 |  memory > explicit_state
                 |  [] > super_state
                 |    memory > hidden_state
                 |
                 |[] > a
                 |  super > @
                 |  memory > state
                 |  [self new_state] > update_state
                 |    self.state.write new_state > @
                 |[] > b
                 |  a > @
                 |  [self var] > alter_func
                 |    var.write "bad" > @
                 |  [self] > bad_func
                 |    self.alter_func self self.state > @
                 |  [self new_state] > change_state_plus_two
                 |    self.bad_func self self.super_state.hidden_state > tmp
                 |    self.explicit_state.write (new_state.add 2) > @
                 |""".stripMargin

    Parser
      .parse(code)
      .flatMap(Inliner.createObjectTree)
      .flatMap(Inliner.resolveParents)
      .flatMap(analyze)
      .leftMap(println)
      .foreach(_.foreach(println))
  }

}
