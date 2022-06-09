package org.polystat.odin.backend.eolang

import cats.syntax.foldable._
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.utils.text.escape
import org.polystat.odin.utils.text.indent

import scala.util.Properties

import EOBndRepr.instances._
import ToEO.ops.ToEOOps
import ToEOBnd.instances._
import ToEOBnd.ops._
import inlineorlines._
import inlineorlines.ops._

trait ToEO[T, R] {
  def toEO(node: T): R
}

object ToEO {
  def apply[T, R](implicit toEO: ToEO[T, R]): ToEO[T, R] = toEO

  object ops {

    def toEO[T, R](node: T)(implicit toEO: ToEO[T, R]): R =
      ToEO[T, R].toEO(node)

    implicit class ToEOOps[T, R](private val node: T) extends AnyVal {
      def toEO(implicit toEO: ToEO[T, R]): R = ToEO[T, R].toEO(node)

      def toEOPretty(implicit toEO: ToEO[T, InlineOrLines]): String =
        ToEO[T, InlineOrLines].toEO(node) match {
          case Lines(lines) => lines.mkString(Properties.lineSeparator)
          case Inline(line) => line
        }

    }

  }

  object instances {

    implicit val eoExprOnlyToEO: ToEO[EOExprOnly, InlineOrLines] =
      new ToEO[EOExprOnly, InlineOrLines] {
        override def toEO(node: EOExprOnly): InlineOrLines = Fix.un(node).toEO
      }

    // Program
    // ///////////////////////////////////////////////////////////////////
    implicit val progToEO: ToEO[EOProg[EOExprOnly], InlineOrLines] =
      new ToEO[EOProg[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOProg[EOExprOnly]): InlineOrLines = {
          val metas = node.metas.toEO.toVector
          val program = node.bnds.flatMap(_.toEO.toVector)
          val newlineAfterMetas =
            if (metas.nonEmpty) Vector("") else Vector()
          val newlineAfterPrograms =
            if (program.nonEmpty) Vector("") else Vector()

          Lines(
            metas ++
              newlineAfterMetas ++
              program ++
              newlineAfterPrograms
          )
        }

      }

    // Metas  //////////////////////////////////////////////////////////////////
    implicit val metasToEO: ToEO[EOMetas, Lines] =
      new ToEO[EOMetas, Lines] {

        override def toEO(node: EOMetas): Lines = Lines(
          node
            .pack
            .map(p => s"${Constants.SYMBS.META_PREFIX}package $p")
            .toVector ++
            node.metas.map(_.toEO.line)
        )

      }

    // Concrete metas //////////////////////////////////////////////////////////
    implicit val metaToEO: ToEO[EOMeta, Inline] =
      new ToEO[EOMeta, Inline] {

        override def toEO(node: EOMeta): Inline = node match {
          case a: EOAliasMeta => a.toEO
          case rt: EORTMeta => rt.toEO
          case other: EOOtherMeta => other.toEO
        }

      }

    implicit val aliasMetaToEO: ToEO[EOAliasMeta, Inline] =
      new ToEO[EOAliasMeta, Inline] {

        override def toEO(node: EOAliasMeta): Inline = {
          val alias = node.alias.map(_ + " ").getOrElse("")
          val source = node.src.mkString_(".")
          Inline(
            s"${Constants.SYMBS.META_PREFIX}alias $alias$source"
          )
        }

      }

    implicit val rtMetaToEO: ToEO[EORTMeta, Inline] =
      new ToEO[EORTMeta, Inline] {

        override def toEO(node: EORTMeta): Inline =
          Inline(s"${Constants.SYMBS.META_PREFIX}rt ${node.rtName} ${node.src}")

      }

    implicit val otherMetaToEO: ToEO[EOOtherMeta, Inline] =
      new ToEO[EOOtherMeta, Inline] {

        override def toEO(node: EOOtherMeta): Inline = Inline(
          s"+${node.head} ${node.tail.mkString(" ")}"
        )

      }

    // Binding name ////////////////////////////////////////////////////////////
    implicit val bndNameToEO: ToEO[BndName, String] =
      new ToEO[BndName, String] {

        override def toEO(node: BndName): String = node match {
          case l: LazyName => l.toEO
          case c: ConstName => c.toEO
        }

      }

    implicit val lazyBndToEO: ToEO[LazyName, String] =
      new ToEO[LazyName, String] {
        override def toEO(node: LazyName): String = node.name
      }

    implicit val constBndToEO: ToEO[ConstName, String] =
      new ToEO[ConstName, String] {

        override def toEO(node: ConstName): String =
          s"${node.name}${Constants.SYMBS.CONST_MOD}"

      }

    // Binding /////////////////////////////////////////////////////////////////
    implicit val bndToEO: ToEO[EOBnd[EOExprOnly], InlineOrLines] =
      new ToEO[EOBnd[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOBnd[EOExprOnly]): InlineOrLines = node match {
          case a: EOAnonExpr[EOExprOnly] => a.toEO
          case b: EOBndExpr[EOExprOnly] => b.toEO
        }

      }

    implicit val anonExprToEO: ToEO[EOAnonExpr[EOExprOnly], InlineOrLines] =
      new ToEO[EOAnonExpr[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOAnonExpr[EOExprOnly]): InlineOrLines =
          node.expr.toEO

      }

    implicit val bndExprToEO: ToEO[EOBndExpr[EOExprOnly], InlineOrLines] =
      new ToEO[EOBndExpr[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOBndExpr[EOExprOnly]): InlineOrLines =
          node.expr.bndToEO(node.bndName.name.toEO)

      }

    // Expression ////////////////////////////////////////////////////////////
    implicit val exprToEO: ToEO[EOExpr[EOExprOnly], InlineOrLines] =
      new ToEO[EOExpr[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOExpr[EOExprOnly]): InlineOrLines =
          node match {
            case o: EOObj[EOExprOnly] => o.toEO
            case a: EOApp[EOExprOnly] => a.toEO
            case d: EOData[EOExprOnly] => d.toEO
          }

      }

    // / Object //////////////////////////////////////////////////////////////
    implicit val objToEO: ToEO[EOObj[EOExprOnly], InlineOrLines] =
      new ToEO[EOObj[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOObj[EOExprOnly]): InlineOrLines = {
          val freeAttrsWithVararg =
            node.freeAttrs.map(_.toEO) ++
              node
                .varargAttr
                .map(va => s"${va.toEO}${Constants.SYMBS.VARARG_MOD}")

          val freeAttrsEO = Vector(
            Constants.SYMBS.FREE_ATTR_DECL_ST ++
              freeAttrsWithVararg.mkString(" ") ++
              Constants.SYMBS.FREE_ATTR_DECL_ED
          )

          val objBody = node.bndAttrs.flatMap(_.toEO.toVector).map(indent)

          Lines(freeAttrsEO ++ objBody)
        }

      }

    // / Application /////////////////////////////////////////////////////////
    implicit val appToEO: ToEO[EOApp[EOExprOnly], InlineOrLines] =
      new ToEO[EOApp[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOApp[EOExprOnly]): InlineOrLines = node match {
          case n: EOSimpleApp[EOExprOnly] => n.toEO: Inline
          case n: EOSimpleAppWithLocator[EOExprOnly] => n.toEO: Inline
          case n: EODot[EOExprOnly] => n.toEO: Inline
          case n: EOCopy[EOExprOnly] => n.toEO
        }

      }

    implicit val simpleAppToEO: ToEO[EOSimpleApp[EOExprOnly], Inline] =
      new ToEO[EOSimpleApp[EOExprOnly], Inline] {

        override def toEO(node: EOSimpleApp[EOExprOnly]): Inline =
          Inline(node.name)

      }

    implicit val simpleAppWithLocatorToEO: ToEO[EOSimpleAppWithLocator[EOExprOnly], Inline] =
      new ToEO[EOSimpleAppWithLocator[EOExprOnly], Inline] {

        override def toEO(node: EOSimpleAppWithLocator[EOExprOnly]): Inline =
          Inline(
            if (node.locator == 0)
              s"$$.${node.name}"
            else
              List
                .fill(node.locator.toInt)("^")
                .appended(node.name)
                .mkString(".")
          )

      }

    implicit val dotToEO: ToEO[EODot[EOExprOnly], Inline] =
      new ToEO[EODot[EOExprOnly], Inline] {

        override def toEO(node: EODot[EOExprOnly]): Inline = {
          Inline(
            List[String](
              renderArgSingleLine(EOAnonExpr(node.src)),
              node.name
            ).mkString(".")
          )
        }

      }

    def renderObjSingleLine(obj: EOObj[EOExprOnly]): Inline = {
      val params: String = Constants.SYMBS.FREE_ATTR_DECL_ST +
        obj.freeAttrs.map(_.name).mkString(" ") +
        obj
          .varargAttr
          .fold[String]("") { vararg =>
            val prefix = if (obj.freeAttrs.isEmpty) "" else " "
            prefix + vararg.name + Constants.SYMBS.VARARG_MOD
          } +
        Constants.SYMBS.FREE_ATTR_DECL_ED

      val bndAttrs: String =
        if (obj.bndAttrs.isEmpty)
          ""
        else {
          " " +
            obj
              .bndAttrs
              .map(bnd => s"(${renderEOBndSingleLine(bnd).line})")
              .mkString(" ")
        }

      Inline(List(params, bndAttrs).mkString)
    }

    def renderAppSingleLine(app: EOApp[EOExprOnly]): Inline = {
      app match {
        case app: EOSimpleApp[EOExprOnly] => simpleAppToEO.toEO(app)
        case awl: EOSimpleAppWithLocator[EOExprOnly] =>
          simpleAppWithLocatorToEO.toEO(awl)
        case dot: EODot[EOExprOnly] => dotToEO.toEO(dot)
        case cp: EOCopy[EOExprOnly] => renderCopySingleLine(cp)
      }
    }

    def renderEOExprSingleLine(expr: EOExprOnly): Inline = {
      Fix.un(expr) match {
        case obj: EOObj[EOExprOnly] => renderObjSingleLine(obj)
        case app: EOApp[EOExprOnly] => renderAppSingleLine(app)
        case data: EOData[EOExprOnly] => renderDataSingleLine(data)
      }
    }

    def renderNamedBndSingleLine(name: EONamedBnd): String = {
      name match {
        case EOAnyNameBnd(name) => name match {
            case LazyName(name) => name
            case ConstName(name) => name + Constants.SYMBS.CONST_MOD
          }
        case EODecoration => Constants.ATTRS.DECORATION
      }
    }

    def renderEOBndSingleLine(bnd: EOBnd[EOExprOnly]): Inline = {
      bnd match {
        case EOAnonExpr(expr) => renderEOExprSingleLine(expr)
        case EOBndExpr(bndName, expr) =>
          Inline(
            List[String](
              renderEOExprSingleLine(expr).line,
              Constants.SYMBS.BINDING,
              renderNamedBndSingleLine(bndName)
            ).mkString(" ")
          )
      }
    }

    def renderArraySingleLine(arr: EOArray[EOExprOnly]): Inline = {
      val elems: String = arr.elems.map(renderArgSingleLine).mkString(" ")
      Inline(s"${Constants.SYMBS.ARRAY_START} $elems")
    }

    def renderDataSingleLine(data: EOData[EOExprOnly]): Inline = {
      data match {
        case arr: EOArray[EOExprOnly] => renderArraySingleLine(arr)
        case d: EORegexData[EOExprOnly] => d.toEO
        case d: EOStrData[EOExprOnly] => d.toEO
        case d: EOIntData[EOExprOnly] => d.toEO
        case d: EOCharData[EOExprOnly] => d.toEO
        case d: EOBoolData[EOExprOnly] => d.toEO
        case d: EOFloatData[EOExprOnly] => d.toEO
        case d: EOBytesData[EOExprOnly] => d.toEO
      }
    }

    def renderArgSingleLine(arg: EOBnd[EOExprOnly]): String =
      arg match {
        case bnd: EOBndExpr[EOExprOnly] =>
          "(" + renderEOBndSingleLine(bnd).line + ")"
        case eoCopy @ EOAnonExpr(_ @Fix(EOCopy(_, _))) =>
          "(" + renderEOBndSingleLine(eoCopy).line + ")"
        case array @ EOAnonExpr(_ @Fix(EOArray(_))) =>
          "(" + renderEOBndSingleLine(array).line + ")"
        case array @ EOAnonExpr(_ @Fix(EOObj(_, _, _))) =>
          "(" + renderEOBndSingleLine(array).line + ")"
        case other => renderEOBndSingleLine(other).line
      }

    def renderCopySingleLine(copy: EOCopy[EOExprOnly]): Inline = {
      val trg: String = renderArgSingleLine(EOAnonExpr(copy.trg))
      val args: String = copy.args.map(renderArgSingleLine).mkString_(" ")

      Inline(List(trg, args).mkString(" "))
    }

    implicit val copyToEO: ToEO[EOCopy[EOExprOnly], InlineOrLines] =
      new ToEO[EOCopy[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOCopy[EOExprOnly]): InlineOrLines = {
          val outerArgsString =
            node.args.toVector.flatMap(_.toEO.toVector).map(indent)

          Fix.un(node.trg) match {
            case EOObj(_, _, _) => renderCopySingleLine(node)
            case trg =>
              Lines(
                renderArgSingleLine(EOAnonExpr(Fix(trg))) +: outerArgsString
              )
          }
        }

      }

    // / Data ////////////////////////////////////////////////////////////////
    implicit val dataToEO: ToEO[EOData[EOExprOnly], InlineOrLines] =
      new ToEO[EOData[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOData[EOExprOnly]): InlineOrLines =
          node match {
            case n: EOBytesData[EOExprOnly] => n.toEO: Inline
            case n: EOStrData[EOExprOnly] => n.toEO: Inline
            case n: EORegexData[EOExprOnly] => n.toEO: Inline
            case n: EOIntData[EOExprOnly] => n.toEO: Inline
            case n: EOFloatData[EOExprOnly] => n.toEO: Inline
            case n: EOCharData[EOExprOnly] => n.toEO: Inline
            case n: EOBoolData[EOExprOnly] => n.toEO: Inline
            case n: EOArray[EOExprOnly] => n.toEO
          }

      }

    implicit val singleByteToEO: ToEO[EOSingleByte, Inline] =
      new ToEO[EOSingleByte, Inline] {

        override def toEO(node: EOSingleByte): Inline =
          Inline("%X".format(node.byte))

      }

    implicit val bytesDataToEO: ToEO[EOBytesData[EOExprOnly], Inline] =
      new ToEO[EOBytesData[EOExprOnly], Inline] {

        override def toEO(node: EOBytesData[EOExprOnly]): Inline =
          Inline(node.bytes.map(_.toEO).mkString_("-"))

      }

    implicit val srtDataToEO: ToEO[EOStrData[EOExprOnly], Inline] =
      new ToEO[EOStrData[EOExprOnly], Inline] {

        override def toEO(node: EOStrData[EOExprOnly]): Inline =
          Inline(
            "\"" + escape('"', node.str) + "\""
          )

      }

    implicit val regexDataToEO: ToEO[EORegexData[EOExprOnly], Inline] =
      new ToEO[EORegexData[EOExprOnly], Inline] {

        // TODO: support suffixes
        override def toEO(node: EORegexData[EOExprOnly]): Inline =
          Inline(s"/${node.regex.regex}/")

      }

    implicit val intDataToEO: ToEO[EOIntData[EOExprOnly], Inline] =
      new ToEO[EOIntData[EOExprOnly], Inline] {

        override def toEO(node: EOIntData[EOExprOnly]): Inline =
          Inline(node.int.toString)

      }

    implicit val floatDataToEO: ToEO[EOFloatData[EOExprOnly], Inline] =
      new ToEO[EOFloatData[EOExprOnly], Inline] {

        override def toEO(node: EOFloatData[EOExprOnly]): Inline =
          Inline(node.num.toString)

      }

    implicit val charDataToEO: ToEO[EOCharData[EOExprOnly], Inline] =
      new ToEO[EOCharData[EOExprOnly], Inline] {

        override def toEO(node: EOCharData[EOExprOnly]): Inline =
          Inline(
            "\'" + escape('\'', node.char.toString) + "\'"
          )

      }

    implicit val boolDataToEO: ToEO[EOBoolData[EOExprOnly], Inline] =
      new ToEO[EOBoolData[EOExprOnly], Inline] {

        override def toEO(node: EOBoolData[EOExprOnly]): Inline =
          Inline(node.bool.toString)

      }

    implicit val arrayDataToEO: ToEO[EOArray[EOExprOnly], InlineOrLines] =
      new ToEO[EOArray[EOExprOnly], InlineOrLines] {

        override def toEO(node: EOArray[EOExprOnly]): InlineOrLines =
          if (node.elems.isEmpty)
            Inline(Constants.SYMBS.ARRAY_START)
          else
            Lines(
              Vector(Constants.SYMBS.ARRAY_START) ++
                node.elems.flatMap(_.toEO.toVector).map(indent)
            )

      }

  }

}
