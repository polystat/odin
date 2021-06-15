package eo.backend.eolang.instances

import eo.backend.eolang.Lines
import monocle.Monocle.index
import monocle.{ Iso, Optional, Setter }
import monocle.macros.{ GenIso, GenPrism }

import eo.backend.eolang.abstractions.EORepr
import eo.backend.eolang.{ Inline, InlineOrLines }
import eo.core.ast.Constants

object EOReprInstances {
  implicit val stringEORepr: EORepr[String, String] =
    new EORepr[String, String] {
      override def bindToName: String => String => String =
        (src: String) => (name: String) => s"${src} ${Constants.SYMBS.BINDING} ${name}"
    }

  implicit val inlineOrLinesEORepr: EORepr[InlineOrLines, String] =
    new EORepr[InlineOrLines, String] {
      def linesStringVectorIso: Iso[Lines, Vector[String]] =
        Iso[Lines, Vector[String]](_.value.toVector)(v => Lines(v.toIterable))
      val firstLineOpt: Optional[Vector[String], String] = index(0)
      def boundExprOpt[S]: String => Setter[S, String] => S => S =
        (name: String) => (s: Setter[S, String]) => s
          .modify(expr => s"${expr} ${Constants.SYMBS.BINDING} ${name}")

      override def bindToName: InlineOrLines => String => InlineOrLines =
        (iol: InlineOrLines) => (name: String) => {
          val inlineOpt =
            GenPrism[InlineOrLines, Inline] andThen
            GenIso[Inline, String]
          val linesOpt =
            GenPrism[InlineOrLines, Lines] andThen
            linesStringVectorIso           andThen
            firstLineOpt

          val bindInline = boundExprOpt(name)(inlineOpt)
          val bindLines = boundExprOpt(name)(linesOpt)

          (bindInline compose bindLines)(iol)
        }
    }
}
