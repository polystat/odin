package org.polystat.odin.analysis.liskov

// import cats.data.EitherNel
import cats.data.EitherT
import cats.data.NonEmptyList
import cats.effect.Sync
import cats.syntax.functorFilter._
import cats.syntax.parallel._
import org.polystat.odin.analysis.unjustifiedassumptions.Analyzer.getMethodsInfo
import org.polystat.odin.analysis.unjustifiedassumptions.Analyzer.extractMethodLogic
import org.polystat.odin.analysis.utils.inlining.Inliner
import org.polystat.odin.analysis.utils.inlining.Inliner.AnalysisInfo
import org.polystat.odin.analysis.utils.inlining.Inliner.ObjectTreeForAnalysis
import org.polystat.odin.analysis.utils.inlining.MethodInfo
import org.polystat.odin.analysis.utils.inlining.MethodInfoForAnalysis
import org.polystat.odin.analysis.utils.logicalextraction.ExtractLogic.checkImplication
import org.polystat.odin.core.ast.EONamedBnd
// import org.polystat.odin.parser.eo.Parser

object Analyzer {

  case class MethodAndContainer(
    method: MethodInfoForAnalysis,
    container: AnalysisInfo,
    name: String
  )

  case class MethodAnalysisInfo(
    name: EONamedBnd,
    childName: String,
    parentName: String,
    parentCtx: Map[EONamedBnd, MethodInfoForAnalysis],
    childCtx: Map[EONamedBnd, MethodInfoForAnalysis],
    parentVersion: MethodInfoForAnalysis,
    childVersion: MethodInfoForAnalysis,
  )

  def convertMethodInfo(info: MethodInfo): MethodInfoForAnalysis =
    MethodInfoForAnalysis(
      selfArgName = info.selfArgName,
      body = info.body,
      depth = info.depth
    )

  def processAnalysisInfo[F[_]](
    method: MethodAnalysisInfo
  )(implicit F: Sync[F]): EitherT[F, NonEmptyList[String], Option[String]] = {

    val methodName = method.name.name.name
    val parentTag = "parent"
    val childTag = "child"

    for {
      parentCtx <-
        EitherT.fromEither[F](getMethodsInfo(parentTag, method.parentCtx))
      childCtx <-
        EitherT.fromEither[F](getMethodsInfo(childTag, method.childCtx))

      parentMethod <- EitherT.fromEither[F](
        extractMethodLogic(
          method.parentVersion.selfArgName,
          parentTag,
          method.parentVersion,
          methodName,
          parentCtx.keySet
        )
      )
      childMethod <- EitherT.fromEither[F](
        extractMethodLogic(
          method.childVersion.selfArgName,
          childTag,
          method.childVersion,
          methodName,
          childCtx.keySet
        )
      )
      res <- checkImplication(
        methodName,
        parentMethod,
        parentCtx,
        childMethod,
        childCtx,
        (name: String) =>
          s"Method $name of object ${method.childName} violates the Liskov substitution principle as compared to version in parent object ${method.parentName}",
        parentTag,
        childTag
      )
    } yield res

  }

  def analyze[F[_]](
    originalTree: Map[EONamedBnd, Inliner.ObjectTreeForAnalysis]
  )(implicit F: Sync[F]): EitherT[F, NonEmptyList[String], List[String]] = {

    def findOriginalMethods(
      methodName: EONamedBnd,
      info: AnalysisInfo
    ): Option[List[MethodAndContainer]] = for {
      parentInfo <- info.parentInfo
      parent <- parentInfo.linkToParent.getOption(originalTree)
      method <- parent.info.allMethods.get(methodName)
      currentMethod =
        MethodAndContainer(
          method = method,
          container = parent.info,
          name = parent.info.name.name.name
        )
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
                  parentName = parentInfo.name,
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
    )(implicit F: Sync[F]): EitherT[F, NonEmptyList[String], List[String]] = {
      val currentRes =
        tree
          .toList
          .parFlatTraverse(t => {
            val (name, tree) = t
            extractAnalysisInfo(name, tree)
              .parTraverse(processAnalysisInfo(_))
          })
          .map(_.flattenOption)
      // .collect {
      //   case Right(Some(value)) => Right(value)
      //   case Left(value) => Left(value)
      // }
      // .toList
      // .parSequence

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

  // def main(args: Array[String]): Unit = {
  //   val code =
  //     """
  //       |[] > base
  //       |  [self x] > util
  //       |    x > @
  //       |  [self] > n
  //       |    self.util self 10 > @
  //       |
  //       |[] > derived
  //       |  base > @
  //       |  [self x] > util
  //       |    seq > @
  //       |      assert (x.less 10)
  //       |      x
  //       |""".stripMargin

  //   println(
  //     Parser
  //       .parse(code)
  //       .flatMap(Inliner.createObjectTree)
  //       .flatMap(Inliner.resolveParents)
  //       .flatMap(Inliner.resolveIndirectMethods)
  //       .map(analyze)
  //       .merge
  //   )
  // }

}
