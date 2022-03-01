package org.polystat.odin.analysis.logicalexprs

import ap.SimpleAPI
import smtlib.printer.RecursivePrinter
import smtlib.theories.Core.And
import smtlib.theories.Ints._
import smtlib.trees.Commands.{Assert, CheckSat, DeclareConst}
import smtlib.trees.Terms.{QualifiedIdentifier, SSymbol, SimpleIdentifier}

import java.io.StringReader

object LogicalExpression {

  def main(args: Array[String]): Unit = {

    val x: QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol("x.a_0")))
    val y: QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol("y")))
    val xDecl = DeclareConst(SSymbol("x.a_0"), IntSort())
    val yDecl = DeclareConst(SSymbol("y"), IntSort())
    val formula: Assert = Assert(
      And(
        GreaterEquals(NumeralLit(1), Add(x, y)),
        LessEquals(Neg(NumeralLit(1)), Add(x, y))
      )
    )
    val prog =
      List(
        xDecl,
        yDecl,
        formula,
        CheckSat()
      )

    val formulaStr: String =
      prog.map(RecursivePrinter.toString).mkString

    println(formulaStr) // (assert (< 0 (+ x y)))

    SimpleAPI.withProver(dumpSMT = true)(p => {
      p.execSMTLIB(new StringReader(formulaStr))
      println(p.partialModel)
      //      println(p.???)
    })

  }

}
