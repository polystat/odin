package eo.sandbox

import cats.effect.{ ExitCode, IO, IOApp }
import scala.util.chaining._
import eo.sandbox.programs.mutualRecursionExample
import eo.backend.eolang.ToEO.instances._
import eo.backend.eolang.ToEO.ops._

object Sandbox extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    exitCode <- IO.pure(ExitCode.Success)
    mutualRecEORepr: String = mutualRecursionExample.toEO.value.mkString("\n")
    _ <- IO(mutualRecEORepr.tap(print))
  } yield exitCode
}
