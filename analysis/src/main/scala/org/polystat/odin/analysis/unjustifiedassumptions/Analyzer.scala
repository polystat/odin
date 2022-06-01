package org.polystat.odin.analysis.unjustifiedassumptions

import cats.data.EitherNel
import cats.data.EitherT
import cats.data.{NonEmptyList => Nel}
import cats.effect.Sync
import cats.syntax.all._
import org.polystat.odin.analysis.utils.inlining.Inliner.AnalysisInfo
import org.polystat.odin.analysis.utils.inlining.MethodInfoForAnalysis
import org.polystat.odin.analysis.utils.inlining.ObjectTree
import org.polystat.odin.analysis.utils.logicalextraction.ExtractLogic.checkImplication
import org.polystat.odin.analysis.utils.logicalextraction.ExtractLogic.extractInfo
import org.polystat.odin.analysis.utils.logicalextraction.ExtractLogic.mkEqualsBndAttr
import org.polystat.odin.analysis.utils.logicalextraction.SMTUtils
import org.polystat.odin.analysis.utils.logicalextraction.SMTUtils.{
  logicInfo,
  orderLets
}
import org.polystat.odin.core.ast._
import smtlib.theories.Core.And
import smtlib.theories.Core.True
import smtlib.theories.Ints.IntSort
import smtlib.trees.Terms._

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
              "before",
              before,
              methodName,
              methodsBefore.keySet
            )
          )
        res2 <- EitherT.fromEither[F](
          extractMethodLogic("after", after, methodName, methodsAfter.keySet)
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
  ): EitherNel[String, Map[EONamedBnd, logicInfo]] = {
    val methodNames = methods.keySet
    methods
      .toList
      .foldLeft[EitherNel[String, Map[EONamedBnd, logicInfo]]](Right(Map())) {
        case (acc, (key, value)) =>
          for {
            acc <- acc
            newVal <- extractMethodLogic(tag, value, key.name.name, methodNames)
          } yield acc.updated(key, newVal)
      }
  }

  def extractMethodLogic(
    tag: String,
    method: MethodInfoForAnalysis,
    name: String,
    availableMethods: Set[EONamedBnd]
  ): EitherNel[String, logicInfo] = {
    val body = method.body
    val depth = List(tag)

    body.bndAttrs.collectFirst { case EOBndExpr(EODecoration, phiExpr) =>
      phiExpr
    } match {
      case Some(_) =>
        val infos = body.bndAttrs.traverse { case EOBndExpr(bndName, expr) =>
          extractInfo(bndName.name.name :: depth, expr, availableMethods)
            .map(info => (bndName, info))
        }
        infos.flatMap(infos =>
          infos.toMap.get(EODecoration) match {
            case Some(resultInfo) =>
              val localInfos = infos.filter {
                case (EODecoration, _) => false
                case _ => true
              }
              val newExists = localInfos.toList.flatMap { case (name, info) =>
                SortedVar(
                  SMTUtils.nameToSSymbol(List(name.name.name), depth),
                  IntSort()
                ) :: info.exists
              }
              val newProperties = localInfos.toList match {
                case _ :: _ :: _ => And(localInfos.map { case (name, info) =>
                    And(
                      info.properties,
                      mkEqualsBndAttr(name, depth, info.value)
                    )
                  })
                case (name, info) :: Nil =>
                  And(
                    info.properties,
                    mkEqualsBndAttr(name, depth, info.value)
                  )
                case Nil => True()
              }

              // FIXME: we are assuming first argument is self (need to check)
              val params = body
                .freeAttrs
                .tail
                .toList
                .map(name => SMTUtils.mkIntVar(name.name, depth))

              val lets =
                localInfos.collect {
                  case (EOAnyNameBnd(LazyName(letName)), letTerm) =>
                    VarBinding(
                      SMTUtils.nameToSSymbol(List(letName), depth),
                      letTerm.value match {
                        case QualifiedIdentifier(
                               SimpleIdentifier(SSymbol("no-value")),
                               _
                             ) => True()
                        case value => value
                      }
                    )
                }.toList

              orderLets(lets, resultInfo.value)
                .flatMap(resultValue => {
                  Right(
                    newExists match {
                      case x :: xs => logicInfo(
                          params,
                          // TODO: IS THIS HOW IT SHOULD BE?????
                          x :: xs,//List.empty,
                          resultValue,
                          Exists(
                            x,
                            xs,
                            And(resultInfo.properties, newProperties)
                          )
                        )
                      case Nil => logicInfo(
                          params,
                          List.empty,
                          resultValue,
                          And(resultInfo.properties, newProperties)
                        )
                    }
                  )
                })

            case None => Left(Nel.one("Impossible happened!"))
          }
        )
      case None =>
        Left(Nel.one(s"Method $name does not have attached @ attribute"))
    }

  }

}
