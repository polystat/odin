package org.polystat.odin.parser.xmir

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Sync}
import cats.implicits._
import org.polystat.odin.core.ast.EOBnd
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.parser.ast_tests.FullProgramExamples
import org.scalatest.Assertion
import org.scalatest.wordspec.AsyncWordSpec

import scala.xml.Elem

class ParserTests extends AsyncWordSpec with AsyncIOSpec {

  def parseEO[F[_]: Sync](code: String): F[Vector[EOBnd[EOExprOnly]]] = {
    sourceCodeEoParser[F]().parse(code).map(_.bnds)
  }

  def parseXMIRFromSeq[F[_]: Sync](
    code: String
  ): F[Vector[EOBnd[EOExprOnly]]] = {
    for {
      xmir <- EOtoXMIR.parse[F](code)
      scalaXML =
        (scala.xml.XML.loadString(xmir) \\ "objects" \ "o")
          .collect { case elem: Elem => elem.toString }
      parsed <- XmirToAst.parseXMIR(scalaXML)
    } yield parsed
  }

  def parseXMIRFromString[F[_]: Sync](
    code: String
  ): F[Vector[EOBnd[EOExprOnly]]] = {
    for {
      xmir <- EOtoXMIR.parse[F](code)
      parsed <- XmirToAst.parseXMIR(xmir)
    } yield parsed
  }

  def compare[F[_]: Sync](code: String): F[Assertion] = {
    for {
      parsedSeq <- parseXMIRFromSeq(code)
      parsedString <- parseXMIRFromString(code)
      parsedEO <- parseEO(code)
    } yield {
      assert(parsedEO == parsedSeq)
      assert(parsedEO == parsedString)
    }
  }

  val code: String =
    """+package sandbox
      |+alias stdout org.eolang.io.stdout
      |+alias sprintf org.eolang.txt.sprintf
      |
      |[] > base
      |  memory > x
      |  [self v] > f
      |    x.write > @
      |      v
      |  [self v] > g
      |    self.f > @
      |      self
      |      v
      |[] > derived
      |  base > @
      |  [self v] > f
      |    self.g > @
      |      self
      |      v
      |[a]
      |  [] > name
      |[]
      |  [] > a
      |  [] > name
      |[a name]
      |add. > zyx
      |  1
      |  2
      |if.
      |  less.
      |    1
      |    2
      |  1
      |  2
      |""".stripMargin

  val divByZero: String =
    """[] > base
      |  2 > a
      |  [self x...] > f
      |    div. > @
      |      x.get 0
      |      self.a
      |[] > derived
      |  base > @
      |  0 > a
      |  ^.^.^.base.f > stuff
      |  $.base > lmao
      |  ^.base > rofl
      |"str" > str
      |'c' > char
      |123
      |123 > one-two-three
      |a > anA
      |a
      |""".stripMargin

  val simple: String =
    """[args...] > main
      |  stdout > out
      |    sprintf
      |      "aboba %d"
      |      div.
      |        1
      |        0
      |1
      |1 > one
      |1 > one!
      |[a b rest...] > one
      |  1 > one
      |  [] > f
      |    aboba > @
      |""".stripMargin

  val verySimple: String =
    """"hello" > world
      |""".stripMargin

  "XMIR parser" should {
    val tests = List(
      "a lot of code" -> code,
      "very simple" -> verySimple,
      "simple" -> simple,
      "division by zero" -> divByZero,
    ) ++ FullProgramExamples.correct.init.map(tc => (tc.label, tc.code))

    tests.foreach { case (label, code) =>
      registerAsyncTest(label) {
        compare[IO](code)
      }
    }
  }

}
