package org.polystat.odin.analysis.mutualrec.advanced

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import cats.{ApplicativeError, MonadError}
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.analysis.data.CallGraph._
import org.polystat.odin.analysis.data.Program._
import org.polystat.odin.analysis.data._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.analysis._

object Analyzer {

  def resolveParent[F[_]](
    progTree: Tree[PartialObject] // only for lookup
  )(
    of: ObjectName, // object whose parent is being resolved, used for error reporting
    maybeParentName: Option[ObjectName], // parent object name
  )(implicit F: MonadError[F, String]): F[Option[ParentInfo]] = {
    // returns:
    // F.pure(None) - parent was not found and should not be found
    // F.raiseError(...) - some exceptional case
    // F.pure(Some(...)) - parent was found
    maybeParentName match {
      case Some(parentName) =>
        progTree
          .find(_.name == parentName)
          .fold[F[Option[ParentInfo]]](
            F.raiseError(
              s"Parent (or decoratee) object \"${parentName.show}\" of object \"${of.show}\" " +
                s"is specified, but not defined in the program!"
            )
          ) {
            case PartialObject(parentObjName, parentCg, maybeParentOfParent) =>
              for {
                parentOfParent <-
                  resolveParent(progTree)(parentObjName, maybeParentOfParent)
                cg <- convertPartialCg(parentObjName, parentCg, parentOfParent)
              } yield Some(
                ParentInfo(
                  name = parentObjName,
                  callGraph =
                    parentOfParent.fold(cg)(_.callGraph.extendWith(cg)),
                  parent = parentOfParent
                )
              )
          }
      case None => F.pure(None)
    }
  }

  def convertPartialCg[F[_]](
    objectName: ObjectName, // where call graph is defined, used for error reporting
    pcg: PartialCallGraph, // call graph to resolve
    maybeParent: Option[ParentInfo] // the parent object to resolve methods from parent obj
  )(implicit F: ApplicativeError[F, String]): F[CallGraph] = {

    def createErrorMsg(methodName: String): String = {
      s"Method \"$methodName\" was called from the object \"${objectName.show}\"," +
        s" although it is not defined there!"
    }

    def resolveCallWithParent(
      parent: ParentInfo
    )(pc: PartialCall): F[MethodName] = {
      pc match {
        case (Some(whereDefined), name) =>
          F.pure(MethodName(whereDefined, name))
        case (None, name) =>
          parent
            .callGraph
            .keySet
            .find(_.name == name)
            .fold[F[MethodName]](parent.parent match {
              case Some(parentOfParent) => parent
                  .callGraph
                  .keySet
                  .find(_.name == name)
                  .fold[F[MethodName]](
                    resolveCallWithParent(parentOfParent)(pc)
                  )(F.pure)
              case None => F.raiseError(createErrorMsg(name))
            })(F.pure)
      }
    }

    def resolveCallNoParent(pc: PartialCall): F[MethodName] = pc match {
      case (Some(whereDefined), name) =>
        F.pure(MethodName(whereDefined, name))
      case (None, name) => F.raiseError(createErrorMsg(name))
    }

    val resolveCall: PartialCall => F[MethodName] = maybeParent match {
      case Some(parent) => resolveCallWithParent(parent)
      case None => resolveCallNoParent
    }
    pcg
      .toList
      .traverse { case (methodName, partialCalls) =>
        partialCalls
          .toList
          .traverse(resolveCall)
          .map(calls => (methodName, calls.toSet))
      }
      .map(_.toMap)
  }

  def restoreObjectFromTree[F[_]](
    progTree: Tree[PartialObject] // only for lookup
  )(tree: Tree[PartialObject])(implicit F: MonadError[F, String]): F[Object] = {
    val PartialObject(partialObjName, pcg, maybeParent) = tree.node
    for {
      parent <- resolveParent[F](progTree)(partialObjName, maybeParent)
      cg <- convertPartialCg[F](partialObjName, pcg, parent)
      nestedObjs <- tree.children.traverse(restoreObjectFromTree[F](progTree))
    } yield Object(
      name = partialObjName,
      parent = parent,
      callGraph = parent.fold(cg)(_.callGraph.extendWith(cg)),
      nestedObjs = nestedObjs
    )
  }

  def buildProgram[F[_]](
    trees: Vector[Tree[PartialObject]]
  )(implicit F: MonadError[F, String]): F[Program] = {

    val dummyObj: PartialObject = PartialObject(
      name = ObjectName(None, "THIS NAME DOESN'T MATTER"),
      parentName = None,
      cg = Map(),
    )

    trees
      .traverse(restoreObjectFromTree[F](Tree(dummyObj, trees.toList)))
      .map(_.toList)
  }

  private[analysis] def produceChains[F[_]: MonadError[*[_], String]](
    prog: EOProg[EOExprOnly]
  ): F[List[CallChain]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield program.findMultiObjectCycles

  def filterCycleShifts(
    ccs: List[(ObjectName, CallChain)]
  ): List[(ObjectName, CallChain)] = {

    val ord = new Ordering[(ObjectName, CallChain)] {
      def compare(
        i: (ObjectName, CallChain),
        j: (ObjectName, CallChain)
      ): Int =
        if (i._2.isShiftOf(j._2)) 1 else 0
    }

    collection.immutable.SortedSet(ccs: _*)(ord).toList
  }

  def analyzeAst[F[_]](
    prog: EOProg[EOExprOnly]
  )(implicit F: MonadError[F, String]): F[List[OdinAnalysisError]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield filterCycleShifts(program.findMultiObjectCyclesWithObject).map {
      case (objName, ccs) =>
        val fancyChain = ccs
          .map(methodName =>
            if (objName == methodName.whereDefined) methodName.show
            else methodName
              .copy(whereDefined = objName)
              .show + s" (was last redefined in \"${methodName.whereDefined.show}\")"
          )
          .mkString(" -> ")

        OdinAnalysisError(s"${objName.show}: $fancyChain")
    }

}
