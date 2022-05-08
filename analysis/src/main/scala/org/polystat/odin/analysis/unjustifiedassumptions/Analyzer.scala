package org.polystat.odin.analysis.unjustifiedassumptions

import org.polystat.odin.analysis.utils.logicalextraction.SMTUtils.Info
import cats.data.{EitherNel, NonEmptyList => Nel}
import cats.syntax.align._
import cats.syntax.traverse._
import org.polystat.odin.analysis.utils.inlining.Inliner.AnalysisInfo
import org.polystat.odin.analysis.utils.inlining.{MethodInfoForAnalysis, ObjectTree}
import org.polystat.odin.analysis.utils.logicalextraction.ExtractLogic.{checkImplication, extractInfo, mkEqualsBndAttr}
import org.polystat.odin.analysis.utils.logicalextraction.SMTUtils
import org.polystat.odin.core.ast._
import smtlib.theories.Core.{And, True}
import smtlib.theories.Ints.IntSort
import smtlib.trees.Terms._

object Analyzer {

  def analyzeObjectTree(
    objs: Map[EONamedBnd, ObjectTree[(AnalysisInfo, AnalysisInfo)]]
  ): EitherNel[String, List[String]] = {
    objs.toList.flatTraverse { case (_, tree) =>
      for {
        currentRes <- checkMethods _ tupled tree.info
        recurseRes <- Analyzer.analyzeObjectTree(tree.children)
      } yield currentRes ++ recurseRes
    }
  }

  def checkMethods(
    infoBefore: AnalysisInfo,
    infoAfter: AnalysisInfo
  ): EitherNel[String, List[String]] = {
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
        methodsBefore <- getMethodsInfo("before", infoBefore.allMethods)
        methodsAfter <- getMethodsInfo("after", infoAfter.allMethods)

        res1 <-
          processMethod("before", before, methodName, methodsBefore.keySet)
        res2 <- processMethod("after", after, methodName, methodsAfter.keySet)
        res <-
          checkImplication(
            methodName,
            res1,
            methodsBefore,
            res2,
            methodsAfter,
            name => s"Method $name is not referentially transparent"
          )
      } yield res.toList
    }
  }

  def getMethodsInfo(
    tag: String,
    methods: Map[EONamedBnd, MethodInfoForAnalysis]
  ): EitherNel[String, Map[EONamedBnd, Info]] = {
    val methodNames = methods.keySet
    methods
      .toList
      .foldLeft[EitherNel[String, Map[EONamedBnd, Info]]](Right(Map())) {
        case (acc, (key, value)) =>
          for {
            acc <- acc
            newVal <- processMethod(tag, value, key.name.name, methodNames)
          } yield acc.updated(key, newVal)
      }
  }

  def processMethod(
    tag: String,
    method: MethodInfoForAnalysis,
    name: String,
    availableMethods: Set[EONamedBnd]
  ): EitherNel[String, Info] = {
    val body = method.body
    val depth = List(tag)

    body.bndAttrs.collectFirst { case EOBndExpr(EODecoration, expr) =>
      expr
    } match {
      case Some(_) => {
        // val arguments = body.freeAttrs.tail // FIXME: we are assuming first
        // argument is self (need to check)
        val infos = body.bndAttrs.traverse {
          case EOBndExpr(bndName, expr) => {
            extractInfo(bndName.name.name :: depth, expr, availableMethods)
              .map(info => (bndName, info))
          }
        }
        infos.flatMap(infos =>
          infos.toMap.get(EODecoration) match {
            case Some(resultInfo) => Right {
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
                val resultValue = lets match {
                  case x :: xs => Let(x, xs, resultInfo.value)
                  case Nil => resultInfo.value
                }

                newExists match {
                  case x :: xs => Info(
                      params,
                      List.empty,
                      resultValue,
                      Exists(x, xs, And(resultInfo.properties, newProperties))
                    )
                  case Nil => Info(
                      params,
                      List.empty,
                      resultValue,
                      And(resultInfo.properties, newProperties)
                    )
                }
              }
            case None => Left(Nel.one("Impossible happened!"))
          }
        )
      }
      case None =>
        Left(Nel.one(s"Method $name does not have attached @ attribute"))
    }

  }
}
