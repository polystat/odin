package org.polystat.odin.analysis.logicalexprs

import ap.SimpleAPI
import smtlib.printer.RecursivePrinter
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.trees.Commands.{Assert, CheckSat}
import smtlib.trees.Terms._

import java.io.StringReader

object LogicalExpression {

  def main(args: Array[String]): Unit = {

    val x: QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol("x")))
    val y: QualifiedIdentifier =
      QualifiedIdentifier(SimpleIdentifier(SSymbol("y")))
//    val xDecl = DeclareConst(SSymbol("x.a_0"), IntSort())
    //    val yDecl = DeclareConst(SSymbol("y"), IntSort())
    val formula: Assert = Assert(
//      Forall(SortedVar(SSymbol("x"), IntSort()), List(),
      Exists(SortedVar(SSymbol("y"), IntSort()), List(),
          And(
            Exists(SortedVar(SSymbol("x"), IntSort()), List(),
            Equals(NumeralLit(10), x)),
            LessEquals(Neg(NumeralLit(1)), y)
          )
        )
//      )
    )
    val prog =
      List(
        //        xDecl,
        //        yDecl,
        //        , List(), NumeralLit(1)),
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
