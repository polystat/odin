package org.polystat.odin.parser.xmir

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Sync}
import cats.implicits._
import fastparse.Parsed
import org.polystat.odin.core.ast.EOBnd
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.MutualRecExample
import org.polystat.odin.parser.fastparse.Parser
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec

class ParserTests extends AsyncFreeSpec with AsyncIOSpec {

  def parseEO(code: String): Option[Vector[EOBnd[EOExprOnly]]] = {
    Parser.parse(code) match {
      case Parsed.Success(value, _) => Some(value.bnds)
      case _: Parsed.Failure => None
    }
  }

  def parseXMIR[F[_]: Sync](
    code: String
  ): F[Option[Vector[EOBnd[EOExprOnly]]]] = {
    for {
      xmir <- EOtoXMIR.parse[F](code)
      parsed <- XMIR.parse(xmir)
    } yield parsed.toOption
  }

  def compare[F[_]: Sync](code: String): F[Assertion] = {
    for {
      parsedXMIR <- parseXMIR[F](code)
      parsedEO = parseEO(code)
    } yield assert(parsedEO.get == parsedXMIR.get)
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
      |  base.^.f > stuff
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

  "XMIR parser" - {
    val tests = List(
      "a lot of code" -> code,
      "very simple" -> verySimple,
      "simple" -> simple,
      "division by zero" -> divByZero,
      "mutual_recursion_example" -> MutualRecExample.code
    )

    tests.foreach { case (label, code) =>
      registerAsyncTest(label) {
        compare[IO](code)
      }
    }

  }

}
