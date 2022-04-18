package org.polystat.odin.analysis.liskov

import ap.SimpleAPI
import ap.SimpleAPI.FunctionalityMode
import org.polystat.odin.analysis.inlining._
import org.polystat.odin.analysis.inlining.Inliner.{
  AnalysisInfo,
  ObjectTreeForAnalysis
}
import org.polystat.odin.core.ast.EONamedBnd
import org.polystat.odin.parser.eo.Parser
import cats.data.{EitherNel, NonEmptyList => Nel}
//import org.polystat.odin.backend.eolang.ToEO.ops._
//import org.polystat.odin.backend.eolang.ToEO.instances.objToEO

import scala.annotation.tailrec
import org.polystat.odin.analysis.logicalexprs.ExtractLogic.{
  getMethodsInfo,
  mkFunDecls,
  processMethod2,
  Info
}
import smtlib.printer.RecursivePrinter
import smtlib.theories.Core.{And, Equals, Implies, True}
import smtlib.trees.Commands.Assert
import smtlib.trees.Terms.{
  Exists,
  Forall,
  QualifiedIdentifier,
  SSymbol,
  SimpleIdentifier
}

import java.io.StringReader
import scala.util.Try
import cats.syntax.parallel._
import cats.syntax.either._

object DetectViolation {

  case class MethodAndContainer(method: MethodInfo, container: AnalysisInfo)

  case class MethodAnalysisInfo(
    name: EONamedBnd,
    childName: String,
    parentCtx: Map[EONamedBnd, MethodInfo],
    childCtx: Map[EONamedBnd, MethodInfo],
    parentVersion: MethodInfo,
    childVersion: MethodInfo,
  )

  def checkImplication(
    containerName: String,
    methodName: String,
    before: Info,
    methodsBefore: Map[EONamedBnd, Info],
    after: Info,
    methodsAfter: Map[EONamedBnd, Info]
  ): EitherNel[String, Option[String]] = {
    (before.forall, after.forall) match {
      case (x :: xs, y :: ys) =>
        val impl = Forall(
          x,
          xs,
          Exists(
            y,
            ys,
            And(
              And(True() :: before.forall.zip(after.forall).map { case (x, y) =>
                Equals(
                  QualifiedIdentifier(SimpleIdentifier(SSymbol(x.name.name))),
                  QualifiedIdentifier(SimpleIdentifier(SSymbol(y.name.name)))
                )
              }),
              Implies(before.properties, after.properties)
            )
          )
        )
        val declsBefore = methodsBefore.toList.flatMap { case (name, info) =>
          mkFunDecls("before", name, info)
        }
        val declsAfter = methodsAfter.toList.flatMap { case (name, info) =>
          mkFunDecls("after", name, info)
        }
        val prog = declsBefore ++ declsAfter ++ List(Assert(impl))
        val formula = prog.map(RecursivePrinter.toString).mkString

        Try(SimpleAPI.withProver(p => {
          val (assertions, functions, constants, predicates) =
            p.extractSMTLIBAssertionsSymbols(
              new StringReader(formula),
              fullyInline = true
            )
          assertions.foreach(p.addAssertion)
          functions
            .keySet
            .foreach(f => p.addFunction(f, FunctionalityMode.NoUnification))
          constants.keySet.foreach(p.addConstantRaw)
          predicates.keySet.foreach(p.addRelation)
          p.checkSat(true)
          p.getStatus(true) match {
            case ap.SimpleAPI.ProverStatus.Sat => Right(None)
            case ap.SimpleAPI.ProverStatus.Unsat => Right(
                Some(
                  s"Method $methodName of object $containerName violates the Liskov substitution principle"
                )
              )
            case err => Left(Nel.one(s"SMT solver failed with error: $err"))
          }
        }))
          .toEither
          .leftMap(ex => Nel.one(ex.getMessage()))
          .flatten

      case _ => Left(Nel.one("Methods with no arguments are not supported"))
    }
  }

  def processAnalysisInfo(
    method: MethodAnalysisInfo
  ): EitherNel[String, Option[String]] = {

    import cats.syntax.bifunctor._

    def processInfo(info: MethodInfo): MethodInfoForAnalysis =
      MethodInfoForAnalysis(body = info.body, depth = info.depth)

    val methodName = method.name.name.name
    val parentInfo = method.parentCtx.map(_.bimap(identity, processInfo))
    val childInfo = method.childCtx.map(_.bimap(identity, processInfo))

//     println("==================================================")
//     println(method.parentVersion.body.toEOPretty)
//     println("==================================================")
//     println(method.childVersion.body.toEOPretty)
//     println("==================================================")

    for {
      parentCtx <- getMethodsInfo("before", parentInfo)
      childCtx <- getMethodsInfo("after", childInfo)

      parentMethod <- processMethod2(
        "before",
        processInfo(method.parentVersion),
        methodName,
        parentCtx.keySet
      )
      childMethod <- processMethod2(
        "after",
        processInfo(method.childVersion),
        methodName,
        childCtx.keySet
      )
      res <- checkImplication(
        method.childName,
        methodName,
        parentMethod,
        parentCtx,
        childMethod,
        childCtx
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
                parentCtx = parentInfo.container.methods,
                parentVersion = parentInfo.method,
                childCtx = child.info.methods,
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
        |    [self x] > f
        |      x.sub 5 > y1
        |      seq > @
        |        assert (x.less 0)
        |        x
        |
        |  [] > aboba
        |    base > @
        |    [self x] > f
        |      seq > @
        |        assert (x.greater 0)
        |        x
        |
        |  [] > bebra
        |    base > @
        |    [self x] > f
        |      seq > @
        |        assert (0.less x)
        |        x
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
