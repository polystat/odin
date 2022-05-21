package org.polystat.odin.analysis.mutualrec.advanced

import cats.ApplicativeError
import cats.data.EitherNel
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.polystat.odin.analysis.ObjectName
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.polystat.odin.analysis.utils.inlining.Inliner
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import scala.annotation.tailrec

object Analyzer {

  def buildObjectTree(
    prog: EOProg[EOExprOnly]
  ): EitherNel[String, Program] = {
    for {
      tree <- Inliner
        .createObjectTree(prog)
        .flatMap(Inliner.resolveParents)
      program <- buildProgramFromObjectTree(tree)
    } yield program
  }

  def buildProgramFromObjectTree(
    objTree: Map[EONamedBnd, Inliner.CompleteObjectTree]
  ): EitherNel[String, Program] = {

    def getParentTree(
      cur: Inliner.CompleteObjectTree
    ): Option[Inliner.CompleteObjectTree] =
      cur.info.parentInfo.flatMap(_.linkToParent.getOption(objTree))

    def recurseCurrentLevel(
      currentLevel: Map[EONamedBnd, Inliner.CompleteObjectTree]
    ): EitherNel[String, Program] = {
      currentLevel.toList.traverse { case (_, tree) =>
        recurse(tree)
      }
    }

    def resolveParentInfoOf(
      cur: Inliner.CompleteObjectTree
    ): EitherNel[String, Option[ParentInfo]] = {
      val parentTree = getParentTree(cur)

      parentTree match {
        case Some(parent) =>
          val parentOfParent = getParentTree(parent)
          (
            resolveParentInfoOf(parent),
            resolveCallgraph(parentOfParent)(parent)
          ).mapN((parentOfParent, cg) =>
            Some(
              ParentInfo(
                name = parent.info.fqn,
                callGraph =
                  parentOfParent.map(_.callGraph.extendWith(cg)).getOrElse(cg),
                parent = parentOfParent
              )
            )
          )
        case None => Right(None)
      }
    }

    @tailrec
    def resolveCall(cur: Inliner.CompleteObjectTree)(
      parentInfo: Option[Inliner.CompleteObjectTree]
    )(methodName: String): EitherNel[String, MethodName] = {
      val curMethods = cur.info.methods.keySet.map(_.name.name)
      val fullMethodName = MethodName(cur.info.fqn, methodName)
      // TODO: resolve calls relative to extended call graph
      if (curMethods.contains(methodName)) {
        fullMethodName.asRight
      } else {
        parentInfo match {
          case Some(parent) =>
            val parentOfParent = getParentTree(parent)
            resolveCall(parent)(parentOfParent)(methodName)
          case None =>
            (s"Method \"$methodName\" was called from the object \"${cur.info.fqn.show}\"," +
              s" although it is not defined there!").leftNel
        }
      }
    }

    def resolveCallgraph(parentInfo: Option[Inliner.CompleteObjectTree])(
      cur: Inliner.CompleteObjectTree
    ): EitherNel[String, CallGraph] = {
      cur
        .info
        .methods
        .toList
        .traverse { case (name, info) =>
          val methodName = MethodName(cur.info.fqn, name.name.name)
          val calls = info
            .calls
            .map(_.methodName)
            .distinct
            .traverse(resolveCall(cur)(parentInfo))
            .map(_.toSet)
          calls.map(calls => (methodName, calls))

        }
        .map(_.toMap)
    }

    def recurse(
      curTree: Inliner.CompleteObjectTree
    ): EitherNel[String, Object] = {
      val parentTree =
        getParentTree(curTree)

      val parentInfo = resolveParentInfoOf(curTree)
      val cg = resolveCallgraph(parentTree)(curTree)
      val nestedObjs = recurseCurrentLevel(curTree.children)
      (
        parentInfo,
        cg,
        nestedObjs,
      ).mapN((parentInfo, cg, nestedObjs) =>
        Object(
          name = curTree.info.fqn,
          parent = parentInfo,
          nestedObjs = nestedObjs,
          callGraph = parentInfo.map(_.callGraph.extendWith(cg)).getOrElse(cg)
        )
      )
    }

    objTree.toList.traverse { case (_, tree) =>
      recurse(tree)
    }
  }

  private def fromEitherNel[F[_], A](
    einel: EitherNel[String, A]
  )(implicit F: ApplicativeError[F, String]): F[A] = {
    val either: Either[String, A] =
      einel.leftMap(_.mkString_(util.Properties.lineSeparator))
    F.fromEither(either)
  }

  private[analysis] def produceChains[F[_]](
    prog: EOProg[EOExprOnly]
  )(implicit F: ApplicativeError[F, String]): F[List[CallChain]] =
    fromEitherNel(buildObjectTree(prog)).map(_.findMultiObjectCycles)

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
  )(implicit F: ApplicativeError[F, String]): F[List[String]] =
    fromEitherNel(buildObjectTree(prog)).map(program =>
      filterCycleShifts(program.findMultiObjectCyclesWithObject).map {
        case (objName, ccs) =>
          val fancyChain = ccs
            .map(methodName =>
              if (objName == methodName.whereDefined) methodName.show
              else methodName
                .copy(whereDefined = objName)
                .show + s" (was last redefined in \"${methodName.whereDefined.show}\")"
            )
            .mkString(" -> ")

          s"${objName.show}: $fancyChain"
      }
    )

}
