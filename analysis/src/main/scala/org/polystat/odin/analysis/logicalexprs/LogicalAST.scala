package org.polystat.odin.analysis.logicalexprs

import smtlib.printer.RecursivePrinter
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.trees.Commands.{CheckSat, DeclareConst}
import smtlib.trees.Terms._

sealed trait LExpr {
  val properties: Term
  val expr: Term
}

sealed case class AttrBnd(content: LExpr, name: String) {
  val identifier: QualifiedIdentifier =
    QualifiedIdentifier(SimpleIdentifier(SSymbol(name)))

  val declaration: DeclareConst = DeclareConst(SSymbol(name), IntSort())

}

sealed case class LObj(attrs: Vector[AttrBnd], phiAttr: LExpr) extends LExpr {
  private val attrProps: Term = And(attrs.map(_.content.properties))
  private val namesCorrespond: Term =
    And(attrs
      .map(attr => Equals(attr.identifier, attr.content.expr))
    )
  private lazy val declarations: Vector[DeclareConst] = {
    val localIdentifiers = attrs.map(_.declaration)

    attrs.flatMap(_.content match {
      case obj: LObj => localIdentifiers ++ obj.declarations
      case _ => Vector()
    })
  }

  lazy val astRepresentation: String = {
    val prog = declarations ++ List(properties, expr, CheckSat())

    prog.map(RecursivePrinter.toString).mkString
  }
  override lazy val properties: Term = And(namesCorrespond, attrProps)
  override val expr: Term = phiAttr.expr
}


// Operations  ////////////////////////////////////////////////////////////
case class LDiv(num: LExpr, denum: LExpr) extends LExpr {
  override lazy val properties: Term =
    And(num.properties, denum.properties, Not(Equals(denum.expr, SNumeral(0))))
  override val expr: Term = Div(num.expr, denum.expr)
}

// Terminals  ////////////////////////////////////////////////////////////
sealed case class LConst(num: BigInt) extends LExpr {
  override val properties: Term = True()
  override val expr: Term = SNumeral(num)
}

sealed case class Attr(name: String) extends LExpr{
  // Todo? possibly make it refer to the props of the contained expr
  // Not sure how to do it though
  override val properties: Term = True()
  override val expr: Term = QualifiedIdentifier(SimpleIdentifier(SSymbol(name)))
}




