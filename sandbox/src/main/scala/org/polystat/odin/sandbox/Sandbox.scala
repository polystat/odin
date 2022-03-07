package org.polystat.odin.sandbox

import cats.effect.{ExitCode, IO, IOApp}
import org.polystat.odin.interop.java.EOOdinAnalyzer

object Sandbox extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = for {
    exitCode <- IO.pure(ExitCode.Success)
    code = """[] > test
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
             |    parent > @
             |    [self y] > f
             |      y > @
             |    [self z] > h
             |      self.g self z > @
             |""".stripMargin
    analyzed <- IO(new EOOdinAnalyzer.EOOdinSourceCodeAnalyzer().analyze(code))
    _ <- IO.println(analyzed)
  } yield exitCode

}
