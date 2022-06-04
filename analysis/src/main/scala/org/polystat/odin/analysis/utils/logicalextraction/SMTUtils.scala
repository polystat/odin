package org.polystat.odin.analysis.utils.logicalextraction

import cats.data.{EitherNel, NonEmptyList}
import org.polystat.odin.core.ast.EONamedBnd
import smtlib.common.Positioned
import smtlib.theories.Core.BoolSort
import smtlib.theories.Core.True
import smtlib.theories.Ints.IntSort
import smtlib.trees.Commands.FunDef
import smtlib.trees.Terms.{Exists, Forall, FunctionApplication, Identifier, Let, QualifiedIdentifier, SNumeral, SSymbol, SimpleIdentifier, SortedVar, Term, VarBinding}

import scala.annotation.tailrec

object SMTUtils {

  final case class LogicInfo(
    forall: List[SortedVar],
    exists: List[SortedVar],
    value: Term,
    properties: Term,
    lets : List[VarBinding] = List()
  )

  def simpleAppToInfo(names: List[String], depth: List[String]): LogicInfo = {
    LogicInfo(
      List.empty,
      List.empty,
      QualifiedIdentifier(SimpleIdentifier(nameToSSymbol(names, depth))),
      True()
    )
  }

  def nameToSSymbol(names: List[String], depth: List[String]): SSymbol = {
    val name = (depth.reverse ++ names).mkString("-")
    SSymbol(s"var-$name")
  }

  def mkValueFunSSymbol(name: String, depth: List[String]): SSymbol = {
    val qualifiedName = (depth.reverse ++ List(name)).mkString("-")
    SSymbol(s"value-of-$qualifiedName")
  }

  def mkValueFunIdent(
    name: String,
    depth: List[String]
  ): QualifiedIdentifier = {
    QualifiedIdentifier(
      SimpleIdentifier(SMTUtils.mkValueFunSSymbol(name, depth))
    )
  }

  def mkPropertiesFunSSymbol(name: String, depth: List[String]): SSymbol = {
    val qualifiedName = (depth.reverse ++ List(name)).mkString("-")
    SSymbol(s"properties-of-$qualifiedName")
  }

  def mkPropertiesFunIdent(
    name: String,
    depth: List[String]
  ): QualifiedIdentifier = {
    QualifiedIdentifier(
      SimpleIdentifier(SMTUtils.mkPropertiesFunSSymbol(name, depth))
    )
  }

  def mkIntVar(name: String, depth: List[String]): SortedVar = {
    SortedVar(SMTUtils.nameToSSymbol(List(name), depth), IntSort())
  }

  def mkFunDefs(
    tag: String,
    name: EONamedBnd,
    info: LogicInfo
  ): List[FunDef] = {
    val valueDef =
      FunDef(
        SMTUtils.mkValueFunSSymbol(name.name.name, List(tag)),
        info.forall,
        IntSort(),
        info.value
      )
    val propertiesDef =
      FunDef(
        SMTUtils.mkPropertiesFunSSymbol(name.name.name, List(tag)),
        info.forall,
        BoolSort(),
        info.properties
      )
    List(valueDef, propertiesDef)
  }

  def tsort[A](edges: Iterable[(A, A)]): (Iterable[A], Map[A, Set[A]]) = {
    @tailrec
    def tsort(
      toPreds: Map[A, Set[A]],
      done: Iterable[A]
    ): (Iterable[A], Map[A, Set[A]]) = {
      val (noPreds, hasPreds) = toPreds.partition { _._2.isEmpty }
      if (noPreds.isEmpty) {
        (done, hasPreds)
      } else {
        val found = noPreds.keys
        tsort(hasPreds.view.mapValues { _ -- found }.toMap, done ++ found)
      }
    }

    val toPred = edges.foldLeft(Map[A, Set[A]]()) { (acc, e) =>
      acc + (e._1 -> acc
        .getOrElse(e._1, Set())) + (e._2 -> (acc.getOrElse(e._2, Set()) + e._1))
    }

    tsort(toPred, Seq())
  }

  def extractMethodDependencies(term: Positioned): List[String] = {
    def recurse(t: Positioned): List[String] = {
      t match {
        case VarBinding(_, term) => recurse(term)
        case Let(x, xs, term) => recurse(term) ++ xs.flatMap(recurse) ++ recurse(x)
        case Forall(_, _, term) => recurse(term)
        case Exists(_, _, term) => recurse(term)
        case FunctionApplication(fun, terms) =>
          List(fun.id.symbol.name) ++ terms.flatMap(recurse)
        case QualifiedIdentifier(Identifier(SSymbol(name), _), _) => List(name)
        case _ => List()
      }
    }

    recurse(term)
  }

  def orderLets(
    lets: List[VarBinding],
    realTerm: Term
  ): EitherNel[String, Term] = {
    def nestLets(binding: List[VarBinding]): Term = {
      binding match {
        case ::(current, next) => Let(current, List(), nestLets(next))
        case Nil => realTerm
      }
    }

    if (lets.isEmpty)
      Right(realTerm)
    else {
      val letGraph = lets
        .map(let => (let, extractMethodDependencies(let.term)))
        .flatMap { case (binding, dependencies) =>
          dependencies.map(depName =>
            (binding, lets.find(_.name.name == depName))
          )
        }
        .collect { case (binding, Some(dep)) => (binding, dep) }
      val (sortedLets, badLets) = tsort(letGraph)

      if (badLets.nonEmpty)
        Left(
          NonEmptyList.one(
            s"The following local binding form a circular dependency: $sortedLets"
          )
        )
      else
        Right(nestLets(sortedLets.toList.reverse))
    }
  }

  def removeProblematicCalls(fun: FunDef, badFuns: Set[SSymbol]): FunDef = {
    def recurse(t: Term): Term = {
      t match {

        case let @ Let(x, xs, term) => let.copy(
          binding = x.copy(term = recurse(x.term)),
          bindings = xs.map( x => x.copy(term = recurse(x.term))),
          term = recurse(term)
        )
        case forAll @ Forall(_, _, term) => forAll.copy(term = recurse(term))
        case exists @ Exists(_, _, term) => exists.copy(term = recurse(term))
        case call @ FunctionApplication(calledFun, terms) =>
          if (badFuns.contains(calledFun.id.symbol)) {
            fun.returnSort match {
              case BoolSort() => True()
              case _ => SNumeral(8008)
            }
          }
          else {
            call.copy(terms = terms.map(recurse))
          }
        case other => other
      }
    }

    val res = fun.copy(body = recurse(fun.body) match {
      case QualifiedIdentifier(Identifier(SSymbol("true"), _), _) =>
        SNumeral(8008)
      case good => good
    })

    res
  }

}
