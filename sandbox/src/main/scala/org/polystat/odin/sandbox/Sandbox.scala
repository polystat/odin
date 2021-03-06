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
                 |""".stripMargin,
    "fourth" -> """
                  |1 > non-existent
                  |[] > a
                  |  non-existent > @
                  |""".stripMargin,
    "fifth" -> """
                 |[] > b
                 |[] > a
                 |  ^.^.b > @
                 |""".stripMargin,
    "sixth" -> """
                 |[] > a
                 |  [self] > f
                 |    self.non-existent self > @
                 |""".stripMargin,
    "seventh" -> """
                   |[] > a
                   |  [self a b] > f
                   |    a.add b > @
                   |  [self] > g
                   |    self.f self 1 > @
                   |  [self] > h
                   |    self.f self 1 2 3 > @
                   |""".stripMargin,
    "eight" -> """[] > a
                 |  memory > state
                 |  [self new_state] > update_state
                 |    self.state.write new_state > @
                 |[] > b
                 |  a > @
                 |  [self new_state] > change_state_plus_two
                 |    self.state.write (new_state.add 2) > @
                 |""".stripMargin,
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
