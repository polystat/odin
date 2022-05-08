package org.polystat.odin.analysis.utils.logicalextraction

import org.polystat.odin.core.ast.EONamedBnd
import smtlib.theories.Core.{BoolSort, True}
import smtlib.theories.Ints.IntSort
import smtlib.trees.Commands.FunDef
import smtlib.trees.Terms.{Exists, Forall, FunctionApplication, Identifier, Let, QualifiedIdentifier, SNumeral, SSymbol, SimpleIdentifier, SortedVar, Term}

import scala.annotation.tailrec


object SMTUtils {
  final case class Info(
                         forall: List[SortedVar],
                         exists: List[SortedVar],
                         value: Term,
                         properties: Term
                       )

  def simpleAppToInfo(names: List[String], depth: List[String]): Info = {
    Info(
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
    QualifiedIdentifier(SimpleIdentifier(SMTUtils.mkValueFunSSymbol(name, depth)))
  }

  def mkPropertiesFunSSymbol(name: String, depth: List[String]): SSymbol = {
    val qualifiedName = (depth.reverse ++ List(name)).mkString("-")
    SSymbol(s"properties-of-$qualifiedName")
  }

  def mkPropertiesFunIdent(
    name: String,
    depth: List[String]
  ): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(SMTUtils.mkPropertiesFunSSymbol(name, depth)))
  }

  def mkIntVar(name: String, depth: List[String]): SortedVar = {
    SortedVar(SMTUtils.nameToSSymbol(List(name), depth), IntSort())
  }

  def mkFunDefs(tag: String, name: EONamedBnd, info: Info): List[FunDef] = {
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

  def extractMethodDependencies(term: Term): List[String] = {
    def recurse(t: Term): List[String] = {
      t match {
        case Let(_, _, term) => recurse(term)
        case Forall(_, _, term) => recurse(term)
        case Exists(_, _, term) => recurse(term)
        case FunctionApplication(fun, terms) =>
          List(fun.id.symbol.name) ++ terms.flatMap(recurse)
        case _ => List()
      }
    }

    recurse(term)
  }

  def removeProblematicCalls(fun: FunDef, badFuns: Set[SSymbol]): FunDef = {
    def recurse(t: Term): Term = {
      t match {
        case let @ Let(_, _, term) => let.copy(term = recurse(term))
        case forAll @ Forall(_, _, term) => forAll.copy(term = recurse(term))
        case exists @ Exists(_, _, term) => exists.copy(term = recurse(term))
        case call @ FunctionApplication(fun, terms) =>
          if (badFuns.contains(fun.id.symbol))
            True()
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