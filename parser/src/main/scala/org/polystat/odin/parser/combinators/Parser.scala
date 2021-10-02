package org.polystat.odin.parser.combinators

import com.github.tarao.nonempty.collection.NonEmpty
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import errors._
import higherkindness.droste.data.Fix
import org.polystat.odin.parser.Utils.createInverseDot

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.{NoPosition, Position, Reader}

@deprecated("Use org.polystat.odin.parser.fastparse package instead", "0.1.2")
object Parser extends Parsers {

  class WorkflowTokenReader(val tokens: Seq[Token]) extends Reader[Token] {
    override def first: Token = tokens.head

    override def atEnd: Boolean = tokens.isEmpty

    override def pos: Position = NoPosition

    override def rest: Reader[Token] = new WorkflowTokenReader(tokens.tail)
  }

  override type Elem = Token

  def parse(tokens: Seq[Token]): Either[ParserError, EOProg[EOExprOnly]] = {
    val reader = new WorkflowTokenReader(tokens)
    phrase(program)(reader) match {
      case Success(result, _) => Right(result)
      case Error(msg, _) => Left(ParserError(msg))
      case Failure(msg, _) => Left(ParserError(msg))
    }
  }

  def apply(code: String): Either[ParsingError, EOProg[EOExprOnly]] =
    for {
      tokens <- Lexer(code)
      ast <- parse(tokens)
    } yield ast

  private def identifier: Parser[IDENTIFIER] = {
    accept("identifier", { case id: IDENTIFIER => id })
  }

  private def phi: Parser[PHI] = {
    accept("phi", { case phi: PHI => phi })
  }

  private def accessibleAttributeName: Parser[ACCESSIBLE_ATTRIBUTE_NAME] =
    accept(
      "accessibleAttributeName",
      { case name: ACCESSIBLE_ATTRIBUTE_NAME =>
        name
      }
    )

  private def literal: Parser[LITERAL] = {
    accept("literal", { case lit: LITERAL => lit })
  }

  private def single_line_comment: Parser[SINGLE_LINE_COMMENT] = {
    accept(
      "single line comment",
      { case comment: SINGLE_LINE_COMMENT =>
        comment
      }
    )
  }

  private def meta: Parser[META] = {
    accept("meta", { case meta: META => meta })
  }

  def commentsOrNewlines: Parser[List[Token]] =
    rep(NEWLINE | single_line_comment)

  def program: Parser[EOProg[EOExprOnly]] = {
    opt(metas) ~ objects ^^ { case metas ~ objs =>
      EOProg(metas.getOrElse(EOMetas(None, Vector())), objs)
    }
  }

  def metas: Parser[EOMetas] = {
    rep1(commentsOrNewlines ~> meta) ^^ { metas =>
      {
        def processOtherMetas(other: List[META]): List[EOMeta] = other match {
          case META(name, text) :: tail if name == "+alias" =>
            val alias :: value :: _ =
              text.split(' ').filterNot(_.isEmpty).toList
            EOAliasMeta(alias, value) :: processOtherMetas(tail)
          case META(name, text) :: tail if name == "+rt" =>
            val rt :: value :: _ = text.split(' ').filterNot(_.isEmpty).toList
            EORTMeta(rt, value) :: processOtherMetas(tail)
          case META(_, _) :: tail => processOtherMetas(tail)
          case Nil => Nil
        }

        val (pkg, otherMetas) = metas.head match {
          case META(name, text) if name == "+package" =>
            (Some(text), metas.tail)
          case META(_, _) => (None, metas)
        }

        EOMetas(pkg, processOtherMetas(otherMetas).toVector)
      }
    }
  }

  def objects: Parser[Vector[EOBnd[EOExprOnly]]] = {
    rep(`object`) ^^
      (objs => objs.toVector)
  }

  def `object`: Parser[EOBnd[EOExprOnly]] = {
    commentsOrNewlines ~> (application | abstraction) <~ commentsOrNewlines
  }

  def application: Parser[EOBnd[EOExprOnly]] = {
    namedApplication | anonApplication
  }

  def simpleApplicationTarget: Parser[EOExprOnly] = {
    val data = literal ^^ {
      case CHAR(value) => Fix[EOExpr](EOCharData(value.charAt(0)))
      case FLOAT(value) => Fix[EOExpr](EOFloatData(value.toFloat))
      case STRING(value) => Fix[EOExpr](EOStrData(value))
      case INTEGER(value) => Fix[EOExpr](EOIntData(value.toInt))
    }

    val attr = accessibleAttributeName ^^ { name =>
      Fix[EOExpr](EOSimpleApp(name.name))
    }

    attr | data
    // TODO: do something about arrays (`*`)
  }

  def applicationTarget: Parser[EOExprOnly] = {
    val attributeChain: Parser[EOExprOnly] =
      simpleApplicationTarget ~ rep1(DOT ~> accessibleAttributeName) ^^ {
        case start ~ attrs =>
          attrs.foldLeft(start)((acc, id) => Fix[EOExpr](EODot(acc, id.name)))
      }
    attributeChain | simpleApplicationTarget
  }

  private val nonEmptyErrorMsg =
    "Managed to parse zero arguments, where 1 or more were required. This is probably a bug."

  def singleLineApplication: Parser[EOExprOnly] = {
    val justTarget = applicationTarget
    val parenthesized = LPAREN ~> singleLineApplication <~ RPAREN

    val horizontalApplicationArgs: Parser[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] =
      for {
        args <- rep1(justTarget | parenthesized)
        maybeNonemptyArgs = NonEmpty.from(args.map(EOAnonExpr(_)).toVector)
        result <- maybeNonemptyArgs
          .map(as => success(as))
          .getOrElse(
            failure(
              nonEmptyErrorMsg
            )
          )
      } yield result
    val justApplication =
      (parenthesized | justTarget) ~ horizontalApplicationArgs ^^ {
        case trg ~ args => Fix[EOExpr](EOCopy(trg, args))
      }

    justApplication | parenthesized | justTarget
  }

  def verticalApplicationArgs: Parser[NonEmpty[EOBnd[EOExprOnly], Vector[EOBnd[EOExprOnly]]]] = {
    INDENT ~> rep1(`object`) <~ DEDENT >> { args =>
      NonEmpty.from(args) match {
        case Some(value) => success(value.toVector)
        case None => failure(nonEmptyErrorMsg)
      }
    }
  }

  def namedApplication: Parser[EOBndExpr[EOExprOnly]] = {
    val noArgs = singleLineApplication ~ name ^^ { case target ~ name =>
      EOBndExpr(name, target)
    }
    val inverseDot = identifier ~ DOT ~ name ~ verticalApplicationArgs ^^ {
      case id ~ _ ~ name ~ args =>
        EOBndExpr(name, createInverseDot(id.name, args))
    }
    val withArgs = singleLineApplication ~ name ~ verticalApplicationArgs ^^ {
      case target ~ name ~ args =>
        EOBndExpr(name, Fix[EOExpr](EOCopy(target, args)))
    }

    inverseDot | withArgs | noArgs
  }

  def anonApplication: Parser[EOAnonExpr[EOExprOnly]] = {
    val noArgs = singleLineApplication ^^ { target =>
      EOAnonExpr(target)
    }
    val inverseDot = identifier ~ DOT ~ verticalApplicationArgs ^^ {
      case id ~ _ ~ args =>
        EOAnonExpr(createInverseDot(id.name, args))
    }
    val withArgs = singleLineApplication ~ verticalApplicationArgs ^^ {
      case target ~ args =>
        EOAnonExpr(Fix[EOExpr](EOCopy(target, args)))
    }
    inverseDot | withArgs | noArgs
  }

  def abstraction: Parser[EOBnd[EOExprOnly]] = {
    namedAbsObj | anonAbsObj
  }

  def anonAbsObj: Parser[EOAnonExpr[EOExprOnly]] = {
    args ~ opt(boundAttrs) ^^ { case (params, vararg) ~ attrs =>
      EOAnonExpr(
        Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
      )
    }
  }

  def namedAbsObj: Parser[EOBndExpr[EOExprOnly]] = {
    args ~ name ~ opt(boundAttrs) ^^ { case (params, vararg) ~ name ~ attrs =>
      EOBndExpr(
        name,
        Fix[EOExpr](EOObj(params, vararg, attrs.getOrElse(Vector())))
      )
    }
  }

  def name: Parser[EONamedBnd] = {
    val lazyName = ASSIGN_NAME ~> identifier ^^
      (id => EOAnyNameBnd(LazyName(id.name)))
    val lazyPhi = ASSIGN_NAME ~> phi ^^
      (_ => EODecoration)
    val constName = ASSIGN_NAME ~> identifier <~ EXCLAMATION_MARK ^^
      (id => EOAnyNameBnd(ConstName(id.name)))

    constName | lazyPhi | lazyName
  }

  def args: Parser[(Vector[LazyName], Option[LazyName])] = {
    val vararg = LBRACKET ~> varargList <~ RBRACKET ^^ { pair =>
      (pair._1, Some(pair._2))
    }
    val noVararg = LBRACKET ~> argList <~ RBRACKET ^^ { vec =>
      (vec, None)
    }
    vararg | noVararg
  }

  def argList: Parser[Vector[LazyName]] = {
    rep(identifier | phi) ^^ { params =>
      params.map(id => LazyName(id.name)).toVector
    }
  }

  def varargList: Parser[(Vector[LazyName], LazyName)] = {
    rep(identifier | phi) <~ DOTS ^^ { ids =>
      (ids.init.map(id => LazyName(id.name)).toVector, LazyName(ids.last.name))
    }
  }

  def boundAttrs: Parser[Vector[EOBndExpr[EOExprOnly]]] = {
    val boundAttr = namedAbsObj | namedApplication
    val attrs = INDENT ~> rep1(
      commentsOrNewlines ~> boundAttr <~ commentsOrNewlines
    ) <~ DEDENT ^^
      (attrs => attrs.toVector)
    val noAttrs = commentsOrNewlines ^^ { _ =>
      Vector()
    }
    attrs | noAttrs
  }

}
