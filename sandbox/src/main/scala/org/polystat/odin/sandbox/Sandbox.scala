package org.polystat.odin.sandbox

import cats.effect.{ExitCode, IO, IOApp}
import org.polystat.odin.interop.java.EOOdinAnalyzer
import cats.syntax.all._
import scala.jdk.CollectionConverters._

object Sandbox extends IOApp {

  val examples = Map(
    "first" -> """[x y z] > test
                 |  x.div (y.add z) > @
                 |""".stripMargin,
    "second" -> """[] > test
                  |  [] > base
                  |    [self v] > n
                  |      v > @
                  |    [self v] > m
                  |      self.n self v > @
                  |  [] > derived
                  |    base > @
                  |    [self v] > n
                  |      self.m self v > @
                  |""".stripMargin,
    "third" -> """[] > test
                 |  [] > parent
                 |    [self x] > f
                 |      x.sub 5 > y1
                 |      seq > @
                 |        assert (0.less y1)
                 |        x
                 |    [self y] > g
                 |      self.f self y > @
                 |    [self z] > h
                 |      z > @
                 |  [] > child
                 |    test.parent > @
                 |    [self y] > f
                 |      y > @
                 |    [self z] > h
                 |      self.g self z > @
                 |""".stripMargin
  )

  override def run(args: List[String]): IO[ExitCode] = for {
    exitCode <- IO.pure(ExitCode.Success)
    _ <- examples.toList.traverse { case (name, code) =>
      IO.println(s"Example $name:") *>
        IO(
          new EOOdinAnalyzer.EOOdinSourceCodeAnalyzer()
            .analyze(code)
            .asScala
            .toList
        )
          .flatMap(IO.println)
    }
  } yield exitCode

}
