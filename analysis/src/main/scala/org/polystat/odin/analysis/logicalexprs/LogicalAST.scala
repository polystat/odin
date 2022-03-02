package org.polystat.odin.analysis.logicalexprs

//import monocle.Lens
//import monocle.macros.GenLens
//import smtlib.printer.RecursivePrinter

//import smtlib.printer.RecursivePrinter
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.trees.Commands._
import smtlib.trees.Terms._

//import scala.annotation.unused

sealed trait LExpr {
  def properties(prefix: String): Term

  def expr(prefix: String): Term
}

sealed case class AttrBnd(content: LExpr, name: String) {

  def identifier(prefix: String): QualifiedIdentifier =
    QualifiedIdentifier(SimpleIdentifier(SSymbol(prefix + name)))

  def declaration(prefix: String): DeclareConst =
    DeclareConst(SSymbol(prefix + name), IntSort())

}

abstract class BaseLObj(attrs: List[AttrBnd], val name: String) extends LExpr {

  // protected val declLens: Lens[DeclareConst, String] =
  // GenLens[DeclareConst](_.name.name)
  protected def attrDeclarations(prefix: String): List[DeclareConst] =
    attrs.map(_.declaration(prefix))

  protected def innerDeclarations(prefix: String): List[DeclareConst] =
    attrs.map(_.content).flatMap {
      case obj: BaseLObj => obj.declarations(prefix + obj.name)
      // TODO properly add methodname
      case call: LCall => call.method.declarations("")
      case _ => List()
    }

  protected def namesCorrespond(prefix: String): Term = {
    attrs.filter {
      case AttrBnd(_: LObj, _) => false
      case _ => true
    } match {
      case one :: Nil =>
        Equals(one.identifier(prefix), one.content.expr(prefix))
      case several if several.nonEmpty =>
        And(
          several
            .map(attr =>
              Equals(attr.identifier(prefix), attr.content.expr(prefix))
            )
        )
      case _ => True()
    }
  }

  protected def attrProps(prefix: String): Term = {
    attrs match {
      case one :: Nil => one.content.properties(prefix)
      case several if several.nonEmpty =>
        And(several.map(_.content.properties(prefix)))
      case _ => True()
    }
  }

  def declarations(prefix: String): List[DeclareConst]
}

final case class LObj(
  attrs: List[AttrBnd],
  phi: Option[LExpr],
  override val name: String
) extends BaseLObj(attrs, name) {

  override def declarations(prefix: String): List[DeclareConst] =
    attrDeclarations(prefix) ++ innerDeclarations(prefix)

  override def expr(prefix: String): Term =
    phi.map(_.expr(prefix)).getOrElse(True())

  override def properties(prefix: String): Term =
    And(
      namesCorrespond(name + prefix),
      attrProps(name + prefix),
      phi.map(_.properties(name + prefix)).getOrElse(True())
    )

}

final case class LMethod(
  attrs: List[AttrBnd],
  arguments: List[Attr],
  phiAttr: LExpr,
  override val name: String
) extends BaseLObj(attrs, name) {

  def adjustedArgDefinitions(prefix: String): List[Term] =
    arguments.map(_.expr(prefix))

  def adjustedArgDeclarations(prefix: String): List[DeclareConst] =
    arguments.map(_.declaration(prefix))

  def declarations(prefix: String): List[DeclareConst] =
    innerDeclarations(prefix) ++ adjustedArgDeclarations(
      prefix
    ) ++ attrDeclarations(prefix)

  override def expr(prefix: String): Term = phiAttr.expr(prefix)

  override def properties(prefix: String): Term =
    And(namesCorrespond(prefix), attrProps(prefix), phiAttr.properties(prefix))

}

final case class LCall(
  method: LMethod,
  argValues: List[LExpr],
  prefix: String = ""
) extends LExpr {

  private val argNamesCorrespond = method
    .adjustedArgDefinitions(prefix)
    .zip(argValues)
    .map { case (name, arg) =>
      Equals(name, arg.expr(prefix))
    }

  override def properties(prefix: String): Term =
    And(
      And(argValues.map(_.properties(prefix))),
      And(argNamesCorrespond),
      method.properties(prefix)
    )

  override def expr(prefix: String): Term = method.expr(prefix)
}

// Operations  ////////////////////////////////////////////////////////////
final case class LAssert(content: LExpr) extends LExpr {

  override def properties(prefix: String): Term =
    And(content.properties(prefix), expr(prefix))

  override def expr(prefix: String): Term = Equals(content.expr(prefix), True())
}

final case class LSeq(expressions: List[LExpr]) extends LExpr {

  override def properties(prefix: String): Term =
    expressions match {
      case one :: Nil => one.properties(prefix)
      case several if several.nonEmpty => And(several.map(_.properties(prefix)))
      case _ => True()
    }

  override def expr(prefix: String): Term = expressions.last.expr(prefix)
}

final case class LDiv(num: LExpr, denum: LExpr) extends LExpr {

  override def properties(prefix: String): Term =
    And(
      num.properties(prefix),
      denum.properties(prefix),
      Not(Equals(denum.expr(prefix), SNumeral(0)))
    )

  override def expr(prefix: String): Term =
    Div(num.expr(prefix), denum.expr(prefix))

}

final case class LAdd(a: LExpr, b: LExpr) extends LExpr {

  override def properties(prefix: String): Term =
    And(a.properties(prefix), b.properties(prefix))

  override def expr(prefix: String): Term = Add(a.expr(prefix), b.expr(prefix))
}

final case class LEq(left: LExpr, right: LExpr) extends LExpr {

  override def properties(prefix: String): Term = {
    val nestedProps = And(left.properties(prefix), right.properties(prefix))

    And(nestedProps, expr(prefix))
  }

  override def expr(prefix: String): Term =
    Equals(left.expr(prefix), right.expr(prefix))

}

final case class LLess(left: LExpr, right: LExpr) extends LExpr {

  override def properties(prefix: String): Term =
    And(left.properties(prefix), right.properties(prefix), expr(prefix))

  override def expr(prefix: String): Term =
    LessThan(left.expr(prefix), right.expr(prefix))

}

final case class LGreater(left: LExpr, right: LExpr) extends LExpr {

  override def properties(prefix: String): Term =
    And(left.properties(prefix), right.properties(prefix), expr(prefix))

  override def expr(prefix: String): Term =
    GreaterThan(left.expr(prefix), right.expr(prefix))

}

//final case class LSqrt(num: LExpr) extends LExpr {
//  override def properties(prefix: String): Term = {
//    val nonNegative = GreaterEquals(num.expr(prefix), SNumeral(0))
//    val nonImaginary = Equals(Mul(this.expr(prefix), this.expr(prefix)), num.expr(prefix))
//
//    And(num.properties(prefix), nonNegative, nonImaginary)
//  }
//
//  // Todo ???
//  override def expr(prefix: String): Term = ???
//}

// Terminals  ////////////////////////////////////////////////////////////
final case class LConst(num: BigInt) extends LExpr {
  override def properties(prefix: String): Term = True()

  override def expr(prefix: String): Term = SNumeral(num)
}

final case class Attr(name: String, fixedPrefix: String = "") extends LExpr {
  // Todo? possibly make it refer to the props of the contained expr
  // Not sure how to do it though
  override def properties(relativePrefix: String): Term = True()

  override def expr(relativePrefix: String): Term = QualifiedIdentifier(
    SimpleIdentifier(SSymbol(relativePrefix + fixedPrefix + name))
  )

  def declaration(relativePrefix: String): DeclareConst =
    DeclareConst(SSymbol(relativePrefix + fixedPrefix + name), IntSort())

}
