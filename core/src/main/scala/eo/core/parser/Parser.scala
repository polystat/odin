package eo.core.parser

import com.github.tarao.nonempty.collection.NonEmpty
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
    application | abstraction
  }

  def application: Parser[EOBnd[EOExprOnly]] = {
    namedApplication | anonApplication
  }

  def namedApplication: Parser[EONamedBnd[EOExprOnly]] = {
    val namedSimpleApp = {
      identifier ~ name ^^ {
        case id ~ name =>
          EONamedBnd(
            name,
            Fix[EOExpr](EOSimpleApp(id.name))
          )
      }
    }

    val namedAttributeChain: Parser[EONamedBnd[EOExprOnly]] =
      identifier ~ rep1(DOT ~> identifier) ~ name ^^ {
        case id ~ lst ~ name => EONamedBnd(
          name,
          lst.foldLeft(
            Fix[EOExpr](EOSimpleApp(id.name))
          )(
            (acc, id) => Fix[EOExpr](EODot(acc, id.name)))
        )
      }

    val namedCopy: Parser[EONamedBnd[EOExprOnly]] = {
      val methodCopy = identifier ~ name ~ INDENT ~ rep1(`object`) <~ DEDENT ^^ {
        case id ~ name ~ _ ~ objs => EONamedBnd(
          name,
          Fix[EOExpr](
            EOCopy(
              Fix[EOExpr](EOSimpleApp(id.name)),
              NonEmpty.from(objs.toVector) match {
                case Some(value) => value
                case None => throw new Exception("Expected 1 or more arguments, passed 0.")
              }
            )
          )
        )
      }
      methodCopy
    }


    namedCopy | namedAttributeChain | namedSimpleApp
  }

  def anonApplication: Parser[EOAnonExpr[EOExprOnly]] = {
    val simpleApp =
      identifier ^^ {
        id =>
          EOAnonExpr(
            Fix[EOExpr](EOSimpleApp(id.name))
          )
      }

    val attributeChain: Parser[EOAnonExpr[EOExprOnly]] =
      identifier ~ rep1(DOT ~> identifier) ^^ {
        case id ~ lst => EOAnonExpr(
          lst.foldLeft(
            Fix[EOExpr](EOSimpleApp(id.name))
          )(
            (acc, id) => Fix[EOExpr](EODot(acc, id.name)))
        )
      }

    val copy: Parser[EOAnonExpr[EOExprOnly]] = {
      val methodCopy = identifier ~ INDENT ~ rep1(`object`) <~ DEDENT ^^ {
        case id ~ _ ~ objs => EOAnonExpr(
          Fix[EOExpr](EOCopy(
            Fix[EOExpr](EOSimpleApp(id.name)),
            NonEmpty.from(objs.toVector) match {
              case Some(value) => value
              case None => throw new Exception("Expected 1 or more arguments, passed 0.")
            }
          ))
        )
      }


      //      val inverseDotCopy = identifier ~ DOT ~ INDENT ~ rep1(`object`) <~ DEDENT ^^ {
      //        case id ~ _ ~ _ ~ objs => ???
      //      }

      methodCopy
    }

    copy | attributeChain | simpleApp
  }

  def abstraction: Parser[EOBnd[EOExprOnly]] = {
    namedAbsObj | anonAbsObj
  }

  def anonAbsObj: Parser[EOAnonExpr[EOExprOnly]] = {
    args ~ opt(boundAttrs) ^^ {
      case (params, vararg) ~ attrs =>
        EOAnonExpr(
          Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
        )
    }
  }

  def namedAbsObj: Parser[EONamedBnd[EOExprOnly]] = {
    args ~ name ~ opt(boundAttrs) ^^ {
      case (params, vararg) ~ name ~ attrs =>
        EONamedBnd(
          name,
          Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
        )
    }
  }

  def name: Parser[EOBndName] = {
    val lazyName = ASSIGN_NAME ~> identifier ^^
      (id => EOAnyName(LazyName(id.name)))
    val lazyPhi = ASSIGN_NAME ~> PHI ^^
      (_ => EODecoration())
    val constName = ASSIGN_NAME ~> identifier <~ EXCLAMATION_MARK ^^
      (id => EOAnyName(ConstName(id.name)))

    constName | lazyPhi | lazyName
  }

  def args: Parser[(Vector[LazyName], Option[LazyName])] = {
    // TODO: include PHI(`@`) into available args
    val vararg = LBRACKET ~> varargList <~ RBRACKET ^^ { pair =>
      (pair._1, Some(pair._2))
    }
    val noVararg = LBRACKET ~> argList <~ RBRACKET ^^ {
      vec => (vec, None)
    }
    vararg | noVararg
  }

  def argList: Parser[Vector[LazyName]] = {
    rep(identifier) ^^ {
      params => params.map(id => LazyName(id.name)).toVector
    }
  }

  def varargList: Parser[(Vector[LazyName], LazyName)] = {
    rep(identifier) <~ DOTS ^^ {
      ids =>
        (
          ids.init.map(id => LazyName(id.name)).toVector,
          LazyName(ids.last.name)
        )
    }
  }

  def boundAttrs: Parser[Vector[EONamedBnd[EOExprOnly]]] = {
    INDENT ~> rep1(namedAbsObj | namedApplication) <~ DEDENT ^^ (attrs => attrs.toVector)
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |+package sandbox
        |+rt jvm java8
        |
        |
        |[] > main
        |  a > namedA
        |  a.b.c > namedC
        |  a > aCopiedWithB
        |    b
        |  a > aCopiedWithBCopiedWithC
        |    b > bCopiedWithC
        |      c > justC
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
