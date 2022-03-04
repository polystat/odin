package org.polystat.odin.analysis.logicalexprs

//import monocle.Lens
//import monocle.macros.GenLens
//import smtlib.printer.RecursivePrinter

//import smtlib.printer.RecursivePrinter

import smtlib.theories.Core._
import smtlib.theories.Ints._
//import smtlib.trees.Commands._
import smtlib.trees.Terms._

//import scala.annotation.unused

sealed trait LExpr {
  def properties(prefix: String): Term

  def expr(prefix: String): Term

  def usedSymbols(prefix: String): List[SSymbol]
}

sealed case class AttrBnd(content: LExpr, name: String) {
  def symbol(prefix: String): SSymbol = SSymbol(prefix + name)

  def identifier(prefix: String): QualifiedIdentifier =
    QualifiedIdentifier(SimpleIdentifier(symbol(prefix)))

//  def declaration(prefix: String): DeclareConst = DeclareConst(SSymbol(prefix + name), IntSort())
}

abstract class BaseLObj(attrs: List[AttrBnd], val name: String) extends LExpr {
  //  protected val declLens: Lens[DeclareConst, String] = GenLens[DeclareConst](_.name.name)
//  protected def attrDeclarations(prefix: String): List[DeclareConst] = attrs.map(_.declaration(prefix))

  protected def innerSymbols(prefix: String): List[SSymbol] =
    attrs.map(_.content).flatMap {
      case obj: BaseLObj => obj.usedSymbols(prefix)
      case call: LCall => call.method.usedSymbols("")
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
          several.map(attr => Equals(attr.identifier(prefix),
            attr.content.expr(prefix)))
        )
      case _ => True()
    }
  }

  protected def attrProps(prefix: String): Term = {
    attrs match {
      case one :: Nil => one.content.properties(prefix)
      case several if several.nonEmpty => And(several.map(_.content.properties(prefix)))
      case _ => True()
    }
  }

}

final case class LObj(attrs: List[AttrBnd], phi: Option[LExpr], override val name: String) extends BaseLObj(attrs, name) {

  override def expr(prefix: String): Term = phi.map(_.expr(prefix)).getOrElse(True())

  override def properties(prfx: String): Term = {
    val prefix =  prfx + name
    usedSymbols(prefix).headOption match {
      case Some(symbol) =>
        Exists(SortedVar(symbol, IntSort()),
          usedSymbols(prefix).tail.map(s => SortedVar(s, IntSort())),
          And(namesCorrespond(prefix), attrProps(prefix), phi.map(_.properties(prefix)).getOrElse(True()))
        )
      case None =>
        And(namesCorrespond(prefix), attrProps(prefix), phi.map(_.properties(prefix)).getOrElse(True()))
    }
  }

  override def usedSymbols(prefix: String): List[SSymbol] =
    innerSymbols(prefix) ++
      attrs.flatMap(_.content.usedSymbols(prefix)) ++
      attrs.map(_.symbol(prefix)) ++
      phi.map(_.usedSymbols(prefix)).getOrElse(List())
}

final case class LMethod(attrs: List[AttrBnd], arguments: List[Attr], phiAttr: LExpr, override val name: String) extends BaseLObj(attrs, name) {
  def adjustedArgSymbols(prefix: String): List[SSymbol] = arguments.flatMap(_.usedSymbols(prefix))
  def adjustedArgDefinitions(prefix: String): List[Term] = arguments.map(_.expr(prefix))

  override def expr(prefix: String): Term = phiAttr.expr(prefix)

  override def properties(prfx: String): Term = {
    val prefix = prfx + name

      usedSymbols(prefix).headOption match {
      case Some(symbol) =>
        Exists(SortedVar(symbol, IntSort()),
          usedSymbols(prefix).tail.map(s => SortedVar(s, IntSort())),
          And(namesCorrespond(prefix), attrProps(prefix), phiAttr.properties(prefix))
        )
      case None =>
        And(namesCorrespond(prefix), attrProps(prefix), phiAttr.properties(prefix))
    }
  }
  def argumentSortedVars(prefix: String): List[SortedVar] =
    arguments.map(a => SortedVar(a.usedSymbols(prefix).head, IntSort()))

  override def usedSymbols(prefix: String): List[SSymbol] = {
    attrs.map(_.symbol(prefix)) ++
      attrs.flatMap(_.content.usedSymbols(prefix)) ++
      innerSymbols(prefix) ++
      phiAttr.usedSymbols(prefix)
  }
}

final case class LCall(method: LMethod, argValues: List[LExpr], prefix: String = "") extends LExpr {
  private val argNamesCorrespond =
    method
    .adjustedArgDefinitions(prefix)
    .zip(argValues)
    .zip(method.arguments.flatMap(_.usedSymbols(prefix)))
    .map {
      case ((name, arg), symbol) =>
        Exists(SortedVar(symbol, IntSort()), List(),
          Equals(name, arg.expr(prefix))
        )
    }

  override def properties(prefix: String): Term = {
    usedSymbols(prefix).headOption match {
      case Some(symbol) =>
        Exists(SortedVar(symbol, IntSort()),
          usedSymbols(prefix).tail.map(s => SortedVar(s, IntSort())),
          And(And(argValues.map(_.properties(prefix))),
            And(argNamesCorrespond),
            method.properties(prefix))
        )
      case None =>
        And(And(argValues.map(_.properties(prefix))),
          And(argNamesCorrespond),
          method.properties(prefix))
    }
  }

  override def expr(prefix: String): Term = method.expr(prefix)

  override def usedSymbols(relativePrefix: String): List[SSymbol] =
    method.usedSymbols(relativePrefix) ++ argValues.flatMap(_.usedSymbols(relativePrefix))
}

// Operations  ////////////////////////////////////////////////////////////
final case class LAssert(content: LExpr) extends LExpr {
  override def properties(prefix: String): Term = And(content.properties(prefix), expr(prefix))

  override def expr(prefix: String): Term = Equals(content.expr(prefix), True())

  override def usedSymbols(relativePrefix: String): List[SSymbol] = content.usedSymbols(relativePrefix)
}

final case class LSeq(expressions: List[LExpr]) extends LExpr {
  override def properties(prefix: String): Term =
    expressions match {
      case one :: Nil => one.properties(prefix)
      case several if several.nonEmpty => And(several.map(_.properties(prefix)))
      case _ => True()
    }

  override def expr(prefix: String): Term = expressions.last.expr(prefix)

  override def usedSymbols(relativePrefix: String): List[SSymbol] = expressions.flatMap(_.usedSymbols(relativePrefix))
}

final case class LDiv(num: LExpr, denum: LExpr) extends LExpr {
  override def properties(prefix: String): Term =
    And(num.properties(prefix), denum.properties(prefix), Not(Equals(denum.expr(prefix), SNumeral(0))))

  override def expr(prefix: String): Term = Div(num.expr(prefix), denum.expr(prefix))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = num.usedSymbols(relativePrefix) ++ denum.usedSymbols(relativePrefix)
}

final case class LAdd(left: LExpr, right: LExpr) extends LExpr {
  override def properties(prefix: String): Term = And(left.properties(prefix), right.properties(prefix))

  override def expr(prefix: String): Term = Add(left.expr(prefix), right.expr(prefix))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = left.usedSymbols(relativePrefix) ++ right.usedSymbols(relativePrefix)
}

final case class LSub(left: LExpr, right: LExpr) extends LExpr {
  override def properties(prefix: String): Term = And(left.properties(prefix), right.properties(prefix))

  override def expr(prefix: String): Term = Sub(left.expr(prefix), right.expr(prefix))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = left.usedSymbols(relativePrefix) ++ right.usedSymbols(relativePrefix)
}

final case class LEq(left: LExpr, right: LExpr) extends LExpr {
  override def properties(prefix: String): Term = {
    val nestedProps = And(left.properties(prefix), right.properties(prefix))

    And(nestedProps, expr(prefix))
  }

  override def expr(prefix: String): Term = Equals(left.expr(prefix), right.expr(prefix))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = left.usedSymbols(relativePrefix) ++ right.usedSymbols(relativePrefix)
}

final case class LLess(left: LExpr, right: LExpr) extends LExpr {
  override def properties(prefix: String): Term = And(left.properties(prefix), right.properties(prefix), expr(prefix))

  override def expr(prefix: String): Term = LessThan(left.expr(prefix), right.expr(prefix))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = left.usedSymbols(relativePrefix) ++ right.usedSymbols(relativePrefix)
}

final case class LGreater(left: LExpr, right: LExpr) extends LExpr {
  override def properties(prefix: String): Term = And(left.properties(prefix), right.properties(prefix), expr(prefix))

  override def expr(prefix: String): Term = GreaterThan(left.expr(prefix), right.expr(prefix))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = left.usedSymbols(relativePrefix) ++ right.usedSymbols(relativePrefix)
}

//final case class LSqrt(num: LExpr) extends LExpr {
//  override def properties(prefix: String): Term = {
//    val nonNegative = GreaterEquals(num.expr(prefix), SNumeral(0))
//    val nonImaginary = Equals(Mul(this.expr(prefix), this.expr(prefix)), num.expr(prefix))
//
//    And(num.properties(prefix), nonNegative, nonImaginary)
//  }
//
//  override def expr(prefix: String): Term = ???
//}

// Terminals  ////////////////////////////////////////////////////////////
final case class LConst(num: BigInt) extends LExpr {
  override def properties(prefix: String): Term = True()

  override def expr(prefix: String): Term = SNumeral(num)

  override def usedSymbols(relativePrefix: String): List[SSymbol] = List()
}

final case class Attr(name: String, fixedPrefix: String = "") extends LExpr {
  override def properties(relativePrefix: String): Term = True()

  override def expr(relativePrefix: String): Term = QualifiedIdentifier(SimpleIdentifier(SSymbol(relativePrefix + fixedPrefix + name)))

  override def usedSymbols(relativePrefix: String): List[SSymbol] = List(SSymbol(relativePrefix + fixedPrefix + name))
}




