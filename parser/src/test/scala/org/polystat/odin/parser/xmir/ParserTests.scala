package org.polystat.odin.parser.xmir

import cats.effect.testing.scalatest.AsyncIOSpec
import cats.effect.{IO, Sync}
import cats.implicits._
import fastparse.Parsed
import org.polystat.odin.core.ast.EOBnd
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.TestUtils.astPrinter
import org.polystat.odin.parser.fastparse.Parser
import org.polystat.odin.xmir.{EOtoXMIR, XMIR}
import org.scalatest.Assertion
import org.scalatest.freespec.AsyncFreeSpec
import org.scalatest.matchers.should.Matchers

class ParserTests extends AsyncFreeSpec with AsyncIOSpec with Matchers {

  def parseEO(code: String): Option[Vector[EOBnd[EOExprOnly]]] = {
    Parser.parse(code) match {
      case Parsed.Success(value, _) => Some(value.bnds)
      case _: Parsed.Failure => None
    }
  }

  def parseXMIR[F[_]: Sync](code: String): F[Vector[EOBnd[EOExprOnly]]] = {
    for {
      xmir <- EOtoXMIR.parse[F](code)
      parsed <- XMIR.parse(xmir)
    } yield parsed
  }

  def compare[F[_]: Sync](code: String): F[Assertion] =
    for {
      parsedXMIR <- parseXMIR(code)
      _ <- Sync[F].delay(astPrinter.pprintln(parsedXMIR))
      parsedEO <- Sync[F].delay(parseEO(code).get)
      _ <- Sync[F].delay(astPrinter.pprintln(parsedEO))
    } yield assert(parsedEO == parsedXMIR)

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
      |  true
      |  false
      |""".stripMargin

  val divByZero: String =
    """[] > base
      |  2.5 > a
      |  [self x...] > f
      |    div. > @
      |      x.get 0
      |      self.a
      |[] > derived
      |  base > @
      |  0 > a
      |  base.^.f > stuff
      |true > aTrue
      |false > aFalse
      |true
      |false
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
    "work like eo parser" in {
      compare[IO](verySimple)
    }
  }

}
