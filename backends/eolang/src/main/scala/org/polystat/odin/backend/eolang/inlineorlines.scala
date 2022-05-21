package org.polystat.odin.backend.eolang

import cats.Show

object inlineorlines {
  sealed trait InlineOrLines
  case class Inline(line: String) extends InlineOrLines
  case class Lines(lines: Vector[String]) extends InlineOrLines

  implicit val inlineShow: Show[Inline] = new Show[Inline] {
    override def show(t: Inline): String = t.line

  }

  object ops {

    implicit class InlineOrLinesOps(private val iol: InlineOrLines)
      extends AnyVal {

      def toVector: Vector[String] = iol match {
        case Inline(line) => Vector(line)
        case Lines(lines) => lines
      }

      def toLines: Lines = Lines(iol.toVector)

      def allLinesToString: String = iol.toVector.mkString("\n")
    }

  }

}
