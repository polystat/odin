package org.polystat.odin.analysis.inlining

import smtlib.trees.Terms._
import smtlib.theories.Ints._
import smtlib.trees.Commands._
import smtlib.theories.Core._

import java.io.StringReader

// import java.io.StringReader
import ap.SimpleAPI
import smtlib.printer.RecursivePrinter

// import scala.util.Properties

object LogicalExpression {

  def main(args: Array[String]): Unit = {

    val x: QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol("x")))
    val y: QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol("y")))
    val xDecl = DeclareConst(SSymbol("x"), IntSort())
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
