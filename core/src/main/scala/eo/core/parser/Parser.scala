package eo.core.parser

import eo.core.ast._
import eo.core.ast.astparams.EOExprOnly
import higherkindness.droste.data.Fix

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

class WorkflowTokenReader(val tokens: Seq[Token]) extends Reader[Token] {
  override def first: Token = tokens.head

  override def atEnd: Boolean = tokens.isEmpty

  override def pos: Position = NoPosition

  override def rest: Reader[Token] = new WorkflowTokenReader(tokens.tail)
}

case class ParserError(msg: String) extends CompilationError


object Parser extends Parsers {
  override type Elem = Token

  def apply(tokens: Seq[Token]): Either[ParserError, EOProg[EOExprOnly]] = {
    val reader = new WorkflowTokenReader(tokens)
    program(reader) match {
      case Success(result, _) => Right(result)
      case Error(msg, _) => Left(ParserError(msg))
      case Failure(msg, _) => Left(ParserError(msg))
    }
  }

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifier", { case id: IDENTIFIER => id })
  }
  //
  //  private def literal: Parser[LITERAL] = {
  //    accept("literal", { case lit: LITERAL => lit })
  //  }

  //  private def single_line_comment: Parser[SINGLE_LINE_COMMENT] = {
  //    accept("single line comment", { case comment: SINGLE_LINE_COMMENT => comment })
  //  }

  private def meta: Parser[META] = {
    accept("meta", { case meta: META => meta })
  }


  def program: Parser[EOProg[EOExprOnly]] = {
    opt(metas) ~ objects ^^ {
      case metas ~ objs =>
        EOProg(
          metas.getOrElse(EOMetas(None, Vector())),
          objs
        )
    }
  }


  def metas: Parser[EOMetas] = {
    rep1(meta) ^^ {
      metas => {

        def processOtherMetas(other: List[META]): List[EOMeta] = other match {
          case META(name, text) :: tail if name == "+alias" =>
            val alias :: value :: _ = text.split(' ').filterNot(_.isEmpty).toList
            EOAliasMeta(alias, value) :: processOtherMetas(tail)
          case META(name, text) :: tail if name == "+rt" =>
            val rt :: value :: _ = text.split(' ').filterNot(_.isEmpty).toList
            EORTMeta(rt, value) :: processOtherMetas(tail)
          case META(_, _) :: tail => processOtherMetas(tail)
          case Nil => Nil
        }

        val (pkg, otherMetas) = metas.head match {
          case META(name, text) if name == "+package" => (Some(text), metas.tail)
          case META(_, _) => (None, metas)
        }

        EOMetas(pkg, processOtherMetas(otherMetas).toVector)
      }
    }
  }

  def objects: Parser[Vector[EOBnd[EOExprOnly]]] = {
    rep(`object`) ^^ {
      objs => objs.toVector
    }
  }

  def `object`: Parser[EOBnd[EOExprOnly]] = {
    abstraction
    //    | application
  }

  def abstraction: Parser[EOBnd[EOExprOnly]] = {
    anonAbsObj | namedAbsObj
  }

  def anonAbsObj: Parser[EOAnonExpr[EOExprOnly]] = {
    args ~ opt(boundAttrs) ^^ {
      case (params, vararg) ~ attrs =>
        EOAnonExpr(
          Fix(EOObj(params, vararg, attrs.getOrElse(Vector())))
        )
    }
  }

  def namedAbsObj: Parser[EOBndExpr[EOExprOnly]] = {
    args ~ name ~ opt(boundAttrs) ^^ {
      case (params, vararg) ~ name ~ attrs =>
        EOBndExpr(
          name,
          Fix(EOObj(params, vararg, attrs.getOrElse(Vector())))
        )
    }
  }

  def name: Parser[EONamedBnd] = {
    val lazyName = ASSIGN_NAME ~> identifier ^^
      (id => EOAnyNameBnd(LazyBnd(id.name)))
    val lazyPhi = ASSIGN_NAME ~> PHI ^^
      (_ => EODecoration())
    val constName = ASSIGN_NAME ~> identifier <~ EXCLAMATION_MARK ^^
      (id => EOAnyNameBnd(ConstBnd(id.name)))

    constName | lazyPhi | lazyName
  }

  def args: Parser[(Vector[LazyBnd], Option[LazyBnd])] = {
    val vararg = LBRACKET ~> varargList <~ RBRACKET ^^ { pair =>
      (pair._1, Some(pair._2))
    }
    val noVararg = LBRACKET ~> argList <~ RBRACKET ^^ {
      vec => (vec, None)
    }
    vararg | noVararg
  }

  def argList: Parser[Vector[LazyBnd]] = {
    rep(identifier) ^^ {
      params => params.map(id => LazyBnd(id.name)).toVector
    }
  }

  def varargList: Parser[(Vector[LazyBnd], LazyBnd)] = {
    rep(identifier) <~ DOTS ^^ {
      ids =>
        (
          ids.init.map(id => LazyBnd(id.name)).toVector,
          LazyBnd(ids.last.name)
        )
    }
  }

  def boundAttrs: Parser[Vector[EOBndExpr[EOExprOnly]]] = {
    INDENT ~> rep1(namedAbsObj) <~ DEDENT ^^ (attrs => attrs.toVector)
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |+package sandbox
        |+rt jvm java8
        |
        |[]
        |  [a b] > @
        |    [ad...] > one2!
        |  [a b] > another
        |    [a b c d...] > another2
        |[a d b] # another anon
        |""".stripMargin
    println(code)
    val tokens = Lexer(code) match {
      case Right(value) => value
      case Left(value) => throw new Exception(value.msg)
    }

    println("\nTOKENS:")
    println(tokens)

    val ast = apply(tokens) match {
      case Right(value) => value
      case Left(value) => throw new Exception(value.msg)

    }

    println("\nAST:")
    println(ast)
  }

}
