package org.polystat.odin.analysis.unjustifiedassumptions

import cats.data.{EitherNel, EitherT, NonEmptyList => Nel}
import cats.effect.Sync
import cats.syntax.all._
import org.polystat.odin.analysis.utils.inlining.Inliner.AnalysisInfo
import org.polystat.odin.analysis.utils.inlining.{
  MethodInfoForAnalysis,
  ObjectTree
}
import org.polystat.odin.analysis.utils.logicalextraction.ExtractLogic.{
  checkImplication,
  extractObjectLogic
}
import org.polystat.odin.analysis.utils.logicalextraction.SMTUtils.LogicInfo
import org.polystat.odin.core.ast._

object Analyzer {

  def analyzeObjectTree[F[_]](
    objs: Map[EONamedBnd, ObjectTree[(AnalysisInfo, AnalysisInfo)]]
  )(implicit F: Sync[F]): EitherT[F, Nel[String], List[String]] = {
    objs.toList.flatTraverse { case (_, tree) =>
      val (before, after) = tree.info
      for {
        currentRes <- checkMethods(before, after)
        recurseRes <- Analyzer.analyzeObjectTree(tree.children)
      } yield currentRes ++ recurseRes
    }
  }

  def checkMethods[F[_]](
    infoBefore: AnalysisInfo,
    infoAfter: AnalysisInfo
  )(implicit F: Sync[F]): EitherT[F, Nel[String], List[String]] = {
    val methodPairs = infoBefore
      .indirectMethods
      .alignWith(infoAfter.indirectMethods)(_.onlyBoth.get)

    methodPairs.toList.flatTraverse { case (name, (before, after)) =>
      val methodName = name.name.name
      // println("==================================================")
      // println(before.body.toEOPretty)
      // println("==================================================")
      // println(after.body.toEOPretty)
      // println("==================================================")
      for {
        methodsBefore <-
          EitherT
            .fromEither[F](getMethodsInfo("before", infoBefore.allMethods))
        methodsAfter <-
          EitherT.fromEither[F](getMethodsInfo("after", infoAfter.allMethods))

        res1 <-
          EitherT.fromEither[F](
            extractMethodLogic(
              before.selfArgName,
              "before",
              before,
              methodName,
              methodsBefore.keySet
            )
          )
        res2 <- EitherT.fromEither[F](
          extractMethodLogic(
            after.selfArgName,
            "after",
            after,
            methodName,
            methodsAfter.keySet
          )
        )
        res <-
          checkImplication[F](
            methodName,
            res1,
            methodsBefore,
            res2,
            methodsAfter,
            name =>
              s"Inlining calls in method $name is not safe: doing so may break the behaviour of subclasses!"
          )
      } yield res.toList
    }
  }

  def getMethodsInfo(
    tag: String,
    methods: Map[EONamedBnd, MethodInfoForAnalysis]
  ): EitherNel[String, Map[EONamedBnd, LogicInfo]] = {
    val methodNames = methods.keySet
    methods
      .toList
      .foldLeft[EitherNel[String, Map[EONamedBnd, LogicInfo]]](Right(Map())) {
        case (acc, (key, value)) =>
          for {
            acc <- acc
            newVal <- extractMethodLogic(
              value.selfArgName,
              tag,
              value,
              key.name.name,
              methodNames
            )
          } yield acc.updated(key, newVal)
      }
  }

  def extractMethodLogic(
    selfArgName: String,
    tag: String,
    method: MethodInfoForAnalysis,
    name: String,
    availableMethods: Set[EONamedBnd]
  ): EitherNel[String, LogicInfo] = {
    val body = method.body

    body.bndAttrs.collectFirst { case EOBndExpr(EODecoration, phiExpr) =>
      phiExpr
    } match {
      case Some(_) =>
        extractObjectLogic(selfArgName, body, availableMethods, List(tag))
      case None =>
        Left(Nel.one(s"Method $name does not have attached @ attribute"))
    }

  }

}
