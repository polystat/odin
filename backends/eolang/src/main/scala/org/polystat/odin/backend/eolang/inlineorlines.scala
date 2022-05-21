package org.polystat.odin.backend.eolang

import cats.Show

object inlineorlines {
  type InlineOrLines = Either[String, Iterable[String]]

  type Inline = Left[String, Iterable[String]]

  implicit val inlineShow: Show[Inline] = new Show[Inline] {
    override def show(t: Inline): String = {
      val Left(s) = t
      s
    }

  }

  val Inline: String => Left[String, Iterable[String]] =
    Left[String, Iterable[String]]

  type Lines = Right[String, Iterable[String]]

  val Lines: Iterable[String] => Right[String, Iterable[String]] =
    Right[String, Iterable[String]]

  object conversions {

    implicit val inlineOrLinesToIterable: InlineOrLines => Iterable[String] = {
      case Left(s) => Vector(s)
      case Right(lines) => lines
    }

  }

  object ops {

    implicit class InlineOrLinesOps(private val iol: InlineOrLines)
      extends AnyVal {
      import conversions._

      def toIterable: Iterable[String] = iol

      def toLines: Lines = Lines(iol)

      def allLinesToString: String = iol.mkString("\n")
    }

  }

}
