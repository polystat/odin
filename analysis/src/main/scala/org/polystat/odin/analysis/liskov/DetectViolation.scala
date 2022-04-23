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

object DetectViolation {

  case class MethodAndContainer(
    method: MethodInfoForAnalysis,
    container: AnalysisInfo
  )

  case class MethodAnalysisInfo(
    name: EONamedBnd,
    childName: String,
    parentCtx: Map[EONamedBnd, MethodInfoForAnalysis],
    childCtx: Map[EONamedBnd, MethodInfoForAnalysis],
    parentVersion: MethodInfoForAnalysis,
    childVersion: MethodInfoForAnalysis,
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
        method.parentVersion,
        methodName,
        parentCtx.keySet
      )
      childMethod <- processMethod2(
        childTag,
        method.childVersion,
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

    def findOriginalMethods(
      methodName: EONamedBnd,
      info: AnalysisInfo
    ): Option[List[MethodAndContainer]] = for {
      parentInfo <- info.parentInfo
      parent <- parentInfo.linkToParent.getOption(originalTree)
      method <- parent.info.allMethods.get(methodName)
      currentMethod =
        MethodAndContainer(method = method, container = parent.info)
      res = findOriginalMethods(methodName, parent.info)
    } yield res.map(_.appended(currentMethod)).getOrElse(List(currentMethod))

    def extractAnalysisInfo(
      childName: EONamedBnd,
      child: ObjectTreeForAnalysis
    ): List[MethodAnalysisInfo] = {
      child
        .info
        .allMethods
        .toList
        .flatMap(t => {
          val (methodName, method) = t

          findOriginalMethods(methodName, child.info) match {
            case Some(infos) => infos.map { parentInfo =>
                MethodAnalysisInfo(
                  name = methodName,
                  childName = childName.name.name,
                  parentCtx = parentInfo.container.allMethods,
                  parentVersion = parentInfo.method,
                  childCtx = child.info.allMethods,
                  childVersion = method
                )
              }
            case None => List()
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
        |[] > parent
        |  [self x] > method
        |    22 > @
        |
        |[] > obj
        |  parent > @
        |  [self x] > method
        |    10.div x > tmp
        |    seq > @
        |      assert (x.less 10)
        |      22
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
