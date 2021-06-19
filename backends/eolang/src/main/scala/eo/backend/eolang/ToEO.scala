package eo.backend.eolang

import cats.implicits.toBifunctorOps
import eo.backend.eolang.EOBndRepr.instances._
import eo.backend.eolang.ToEO.ops.ToEOOps
import eo.backend.eolang.ToEOBnd.instances._
import eo.backend.eolang.ToEOBnd.ops._
import eo.backend.eolang.inlineorlines._
import eo.backend.eolang.inlineorlines.ops._
import eo.core.ast._
import eo.utils.text._

trait ToEO[T, R] {
  def toEO(node: T): R
}

object ToEO {
  def apply[T, R](implicit toEO: ToEO[T, R]): ToEO[T, R] = toEO

  object ops {
    def toEO[T, R](node: T)(implicit toEO: ToEO[T, R]): R = ToEO[T, R].toEO(node)

    implicit class ToEOOps[T, R](val node: T) extends AnyVal {
      def toEO(implicit toEO: ToEO[T, R]): R = ToEO[T, R] toEO (node)
    }
  }

  object instances {
    // General /////////////////////////////////////////////////////////////////
//    implicit def InlineToEO[T](implicit toEOInline: ToEO[T, Inline]): ToEO[T, InlineOrLines] =
//      new ToEO[T, InlineOrLines] {
//        override def toEO(node: T): InlineOrLines = toEOInline.toEO(node)
//      }
//
//    implicit def LinesToEO[T](implicit toEOInline: ToEO[T, Lines]): ToEO[T, InlineOrLines] =
//      new ToEO[T, InlineOrLines] {
//        override def toEO(node: T): InlineOrLines = toEOInline.toEO(node)
//      }

    // Program ///////////////////////////////////////////////////////////////////
    implicit val progToEO: ToEO[EOProg, Lines] =
      new ToEO[EOProg, Lines] {
        override def toEO(node: EOProg): Lines = {
          val metas = node.metas.toEO.toIterable
          val program = node.bnds.flatMap(_.toEO.toIterable)

          Lines(metas ++ program)
        }
      }

    // Metas  //////////////////////////////////////////////////////////////////
    implicit val metasToEO: ToEO[EOMetas, Lines] =
      new ToEO[EOMetas, Lines] {
        override def toEO(node: EOMetas): Lines = Lines(
          node.pack.map(p => s"${Constants.SYMBS.META_PREFIX}package ${p}") ++
          node.metas.map(_.toEO.value)
        )
      }

    // Concrete metas //////////////////////////////////////////////////////////
    implicit val metaToEO: ToEO[EOMeta, Inline] =
      new ToEO[EOMeta, Inline] {
        override def toEO(node: EOMeta): Inline = node match {
          case a: EOAliasMeta => a.toEO
          case rt: EORTMeta   => rt.toEO
        }
      }

    implicit val aliasMetaToEO: ToEO[EOAliasMeta, Inline] =
      new ToEO[EOAliasMeta, Inline] {
        override def toEO(node: EOAliasMeta): Inline =
          Inline(s"${Constants.SYMBS.META_PREFIX}alias ${node.alias} ${node.src}")
      }

    implicit val rtMetaToEO: ToEO[EORTMeta, Inline] =
      new ToEO[EORTMeta, Inline] {
        override def toEO(node: EORTMeta): Inline =
          Inline(s"${Constants.SYMBS.META_PREFIX}rt ${node.rtName} ${node.src}")
      }


    // Binding name ////////////////////////////////////////////////////////////
    implicit val bndNameToEO: ToEO[BndName, String] =
      new ToEO[BndName, String] {
        override def toEO(node: BndName): String = node match {
          case l: LazyBnd  => l.toEO
          case c: ConstBnd => c.toEO
        }
      }

    implicit val lazyBndToEO: ToEO[LazyBnd, String] =
      new ToEO[LazyBnd, String] {
        override def toEO(node: LazyBnd): String = node.name
      }

    implicit val constBndToEO: ToEO[ConstBnd, String] =
      new ToEO[ConstBnd, String] {
        override def toEO(node: ConstBnd): String = s"${node.name}${Constants.SYMBS.CONST_MOD}"
      }

    // Binding /////////////////////////////////////////////////////////////////
    implicit val bndToEO: ToEO[EOBnd, InlineOrLines] =
      new ToEO[EOBnd, InlineOrLines] {
        override def toEO(node: EOBnd): InlineOrLines = node match {
          case a: EOAnonExpr => a.toEO
          case b: EOBndExpr  => b.toEO
        }
      }

    implicit val anonExprToEO: ToEO[EOAnonExpr, InlineOrLines] =
      new ToEO[EOAnonExpr, InlineOrLines] {
        override def toEO(node: EOAnonExpr): InlineOrLines = node.expr.toEO
      }

    implicit val bndExprToEO: ToEO[EOBndExpr, InlineOrLines] =
      new ToEO[EOBndExpr, InlineOrLines] {
        override def toEO(node: EOBndExpr): InlineOrLines =
          node.expr.bndToEO(node.bndName.name.toEO)
      }

      // Expression ////////////////////////////////////////////////////////////
      implicit val exprToEO: ToEO[EOExpr, InlineOrLines] =
        new ToEO[EOExpr, InlineOrLines] {
          override def toEO(node: EOExpr): InlineOrLines = node match {
            case o: EOObj  => o.toEO
            case a: EOApp  => a.toEO
            case d: EOData => d.toEO
          }
        }

      // / Object //////////////////////////////////////////////////////////////
      implicit val objToEO: ToEO[EOObj, InlineOrLines] =
        new ToEO[EOObj, InlineOrLines] {
          override def toEO(node: EOObj): InlineOrLines = {
            val freeAttrsWithVararg =
              node.freeAttrs.map(_.toEO) ++
              node.varargAttr.map(va => s"${va.toEO}${Constants.SYMBS.VARARG_MOD}")

            val freeAttrsEO = Vector(
              Constants.SYMBS.FREE_ATTR_DECL_ST ++
              freeAttrsWithVararg.mkString(" ") ++
              Constants.SYMBS.FREE_ATTR_DECL_ED
            )

            val objBody = node.bndAttrs.flatMap(_.toEO.toIterable).map(indent)

            Lines(freeAttrsEO ++ objBody)
          }
        }

      // / Application /////////////////////////////////////////////////////////
      implicit val appToEO: ToEO[EOApp, InlineOrLines] =
        new ToEO[EOApp, InlineOrLines] {
          override def toEO(node: EOApp): InlineOrLines = node match {
            case n: EOSimpleApp => n.toEO : Inline
            case n: EODot       => n.toEO
            case n: EOCopy      => n.toEO : Lines
          }
        }

      implicit val simpleAppToEO: ToEO[EOSimpleApp, Inline] =
        new ToEO[EOSimpleApp, Inline] {
          override def toEO(node: EOSimpleApp): Inline = Inline(node.name)
        }

      implicit val dotToEO: ToEO[EODot, InlineOrLines] =
        new ToEO[EODot, InlineOrLines] {
          def usualDotNotation(n: String)(s: String): String = s"${s}.${n}"

          def reverseDotNotation(n: String)(ls: Iterable[String]): Iterable[String] =
            Vector(s"${n}.") ++ ls.map(indent)

          def dotNotation(n: String)(eoExpr: InlineOrLines): InlineOrLines =
            eoExpr.bimap(
              usualDotNotation(n),
              reverseDotNotation(n)
            )

          def objCases(name: String)(obj: EOObj): InlineOrLines = dotNotation(name)(obj.toEO)

          def appCases(name: String)(app: EOApp): InlineOrLines = app match {
            case n: EOSimpleApp => dotNotation(name)(n.toEO: Inline)
            case n: EODot       => dotNotation(name)(n.src.toEO)
            case n: EOCopy      => dotNotation(name)(n.toEO: Lines)
          }

          def dataCases(name: String)(data: EOData): InlineOrLines = dotNotation(name)(data.toEO)

          override def toEO(node: EODot): InlineOrLines = {
            node.src match {
              case n: EOObj  => objCases(node.name)(n)
              case n: EOApp  => appCases(node.name)(n)
              case n: EOData => dataCases(node.name)(n)
            }
          }
        }

      implicit val copyToEO: ToEO[EOCopy, Lines] =
        new ToEO[EOCopy, Lines] {
          override def toEO(node: EOCopy): Lines = Lines(
            node.trg.toEO.toIterable ++
              node.args.flatMap(_.toEO.toIterable).map(indent)
          )
        }

      // / Data ////////////////////////////////////////////////////////////////
      implicit val dataToEO: ToEO[EOData, InlineOrLines] =
        new ToEO[EOData, InlineOrLines] {
          override def toEO(node: EOData): InlineOrLines = node match {
            case n: EOBytesData => n.toEO : Inline
            case n: EOStrData   => n.toEO : Inline
            case n: EORegexData => n.toEO : Inline
            case n: EOIntData   => n.toEO : Inline
            case n: EOFloatData => n.toEO : Inline
            case n: EOCharData  => n.toEO : Inline
            case n: EOBoolData  => n.toEO : Inline
            case n: EOArray     => n.toEO
          }
        }

      implicit val singleByteToEO: ToEO[EOSingleByte, Inline] =
        new ToEO[EOSingleByte, Inline] {
          override def toEO(node: EOSingleByte): Inline = Inline(node.byte.formatted("%X"))
        }

      implicit val bytesDataToEO: ToEO[EOBytesData, Inline] =
        new ToEO[EOBytesData, Inline] {
          override def toEO(node: EOBytesData): Inline =
            Inline(node.bytes.map(_.toEO).mkString("-"))
        }

      implicit val srtDataToEO: ToEO[EOStrData, Inline] =
        new ToEO[EOStrData, Inline] {
          override def toEO(node: EOStrData): Inline = Inline(s"\"${node.str}\"")
        }

      implicit val regexDataToEO: ToEO[EORegexData, Inline] =
        new ToEO[EORegexData, Inline] {
          // TODO: support suffixes
          override def toEO(node: EORegexData): Inline = Inline(s"/${node.regex.regex}/")
        }

      implicit val intDataToEO: ToEO[EOIntData, Inline] =
        new ToEO[EOIntData, Inline] {
          override def toEO(node: EOIntData): Inline = Inline(node.int.toString)
        }

      implicit val floatDataToEO: ToEO[EOFloatData, Inline] =
        new ToEO[EOFloatData, Inline] {
          override def toEO(node: EOFloatData): Inline = Inline(node.num.toString)
        }

      implicit val charDataToEO: ToEO[EOCharData, Inline] =
        new ToEO[EOCharData, Inline] {
          override def toEO(node: EOCharData): Inline = Inline(s"'${node.char}'")
        }

      implicit val boolDataToEO: ToEO[EOBoolData, Inline] =
        new ToEO[EOBoolData, Inline] {
          override def toEO(node: EOBoolData): Inline = Inline(node.bool.toString)
        }

      implicit val arrayDataToEO: ToEO[EOArray, InlineOrLines] =
        new ToEO[EOArray, InlineOrLines] {
          override def toEO(node: EOArray): InlineOrLines =
            if (node.elems.isEmpty)
              Inline(Constants.SYMBS.ARRAY_START)
            else
              Lines(
                Vector(Constants.SYMBS.ARRAY_START) ++
                  node.elems.flatMap(_.toEO.toIterable).map(indent)
              )
        }
    }
}
