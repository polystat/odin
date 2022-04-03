package org.polystat.odin.analysis.stateaccess

import cats.data.EitherNel
import org.polystat.odin.analysis.inlining.{Abstract, BndItself, Inliner, MethodInfo, ObjectInfo, ParentInfo}
import org.polystat.odin.core.ast.{EOAnyNameBnd, EOBndExpr, EOCopy, EODot, EONamedBnd, EOSimpleAppWithLocator, LazyName}
import org.polystat.odin.parser.eo.Parser

object DetectAccess {

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
            case BndItself(EOBndExpr(bndName, EOSimpleAppWithLocator("memory", _)))
                 if !existingState.contains(bndName) =>
              bndName
          }

        List(State(container = parentObj.info.name, states = currentStates)) ++
          accumulateParentState(tree)(parentObj.info.parentInfo, existingState ++ currentStates)

      case None => List()
    }
  }

  def getAlteredState(method: (EONamedBnd, MethodInfo)): List[StateChange] = {
    val binds = method._2.body.bndAttrs

    Abstract.foldAst[List[StateChange]](binds) {
      case EOCopy(EODot(EODot(EOSimpleAppWithLocator("self", x), state), "write"), _) if x == 0 =>
        List(StateChange(method._1, EOAnyNameBnd(LazyName(state))))
    }
  }

  def analyze[F[_]](
    tree: Map[EONamedBnd, Inliner.CompleteObjectTree]
  ): EitherNel[String, List[String]] = {
    Right(
      tree
        // Has a parent
        .filter(_._2.info.parentInfo.nonEmpty)
        .flatMap(obj => {
          val availableParentStates =
            accumulateParentState(tree)(obj._2.info.parentInfo)
          val alteredStates = obj._2.info.methods.flatMap(getAlteredState)

          for {
            change <- alteredStates
            State(container, states) <- availableParentStates
          } yield
            if (states.contains(change.state))
              List(
                f"Method '${change.method.name.name}' of object '${obj._1.name.name}' directly accesses state '${change.state.name.name}' of base class '${container.name.name}'"
              )
            else List()
        })
        .flatten
        .toList
    )
  }

  def main(args: Array[String]): Unit = {
    val code = """[] > a
                 |  memory > state
                 |  [self new_state] > update_state
                 |    self.state.write new_state > @
                 |[] > b
                 |  a > @
                 |  [self new_state] > change_state_plus_two
                 |    self.state.write (new_state.add 2) > @
                 |""".stripMargin

    println(
      Parser
        .parse(code)
        .flatMap(Inliner.createObjectTree)
        .flatMap(Inliner.resolveParents)
        .flatMap(analyze)
    )
  }

}
