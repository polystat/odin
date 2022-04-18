package org.polystat.odin.analysis.liskov

import cats.data.EitherNel
import org.polystat.odin.analysis.inlining.Inliner.{
  AnalysisInfo,
  ObjectTreeForAnalysis
}
import org.polystat.odin.analysis.inlining._
import org.polystat.odin.analysis.logicalexprs.ExtractLogic.checkImplication2
import org.polystat.odin.core.ast.EONamedBnd
import org.polystat.odin.parser.eo.Parser
//import org.polystat.odin.backend.eolang.ToEO.ops._
//import org.polystat.odin.backend.eolang.ToEO.instances.objToEO

import cats.syntax.parallel._
import org.polystat.odin.analysis.logicalexprs.ExtractLogic.{
  getMethodsInfo,
  processMethod2
}

import scala.annotation.tailrec

object DetectViolation {

  case class MethodAndContainer(method: MethodInfo, container: AnalysisInfo)

  case class MethodAnalysisInfo(
    name: EONamedBnd,
    childName: String,
    parentCtx: Map[EONamedBnd, MethodInfoForAnalysis],
    childCtx: Map[EONamedBnd, MethodInfoForAnalysis],
    parentVersion: MethodInfo,
    childVersion: MethodInfo,
  )

  def convertMethodInfo(info: MethodInfo): MethodInfoForAnalysis =
    MethodInfoForAnalysis(body = info.body, depth = info.depth)

  def processAnalysisInfo(
    method: MethodAnalysisInfo
  ): EitherNel[String, Option[String]] = {

    val methodName = method.name.name.name
    val parentTag = "parent"
    val childTag = "child"

//     println("==================================================")
//     println(method.parentVersion.body.toEOPretty)
//     println("==================================================")
//     println(method.childVersion.body.toEOPretty)
//     println("==================================================")

    for {
      parentCtx <- getMethodsInfo(parentTag, method.parentCtx)
      childCtx <- getMethodsInfo(childTag, method.childCtx)

      parentMethod <- processMethod2(
        parentTag,
        convertMethodInfo(method.parentVersion),
        methodName,
        parentCtx.keySet
      )
      childMethod <- processMethod2(
        childTag,
        convertMethodInfo(method.childVersion),
        methodName,
        childCtx.keySet
      )
      res <- checkImplication2(
        methodName,
        parentMethod,
        parentCtx,
        childMethod,
        childCtx,
        (name: String) =>
          s"Method $name of object ${method.childName} violates the Liskov substitution principle",
        parentTag,
        childTag
      )
    } yield res

  }

  def analyze[F[_]](
    originalTree: Map[EONamedBnd, Inliner.ObjectTreeForAnalysis]
  ): EitherNel[String, List[String]] = {

    @tailrec
    def findOriginalMethod(
      methodName: EONamedBnd,
      info: AnalysisInfo
    ): Option[MethodAndContainer] =
      info.parentInfo match {
        case Some(parentLink) =>
          parentLink.linkToParent.getOption(originalTree) match {
            case Some(parent) =>
              parent
                .info
                .methods
                .get(methodName) match {
                case Some(method) =>
                  Some(
                    MethodAndContainer(method = method, container = parent.info)
                  )
                case None =>
                  findOriginalMethod(methodName, parent.info)
              }
            case None => None
          }
        case None => None
      }

    def extractAnalysisInfo(
      childName: EONamedBnd,
      child: ObjectTreeForAnalysis
    ): List[MethodAnalysisInfo] = {
      child
        .info
        .methods
        .toList
        .flatMap(t => {
          val (methodName, method) = t

          findOriginalMethod(methodName, child.info)
            .map { parentInfo =>
              MethodAnalysisInfo(
                name = methodName,
                childName = childName.name.name,
                parentCtx = parentInfo.container.allMethods,
                parentVersion = parentInfo.method,
                childCtx = child.info.allMethods,
                childVersion = method
              )
            }
        })

    }

    def recurse(
      tree: Map[EONamedBnd, Inliner.ObjectTreeForAnalysis]
    ): EitherNel[String, List[String]] = {
      val currentRes =
        tree
          .flatMap(t => {
            (extractAnalysisInfo _)
              .tupled(t)
              .map(processAnalysisInfo)
          })
          .collect {
            case Right(Some(value)) => Right(value)
            case Left(value) => Left(value)
          }
          .toList
          .parSequence

      tree
        .values
        .map(_.children)
        .map(recurse)
        .toList
        .appended(currentRes)
        .parFlatSequence
    }

    recurse(originalTree)
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |[] > test
        |  [] > base
        |    [self v] > n
        |      2 > @
        |    [self v] > m
        |      self.n self v > @
        |  [] > derived
        |    base > @
        |    [self v] > n
        |      self.m self v > @
        |
        |""".stripMargin

    println(
      Parser
        .parse(code)
        .flatMap(Inliner.createObjectTree)
        .flatMap(Inliner.resolveParents)
        .flatMap(Inliner.resolveIndirectMethods)
        .map(analyze)
        .merge
    )

  }

}
