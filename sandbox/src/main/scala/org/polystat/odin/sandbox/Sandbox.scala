package org.polystat.odin.sandbox

import cats.effect.{ ExitCode, IO, IOApp, Resource }
import cats.implicits._
import org.polystat.odin.analysis.mutualrec.naive.mutualrec.{ findMutualRecursionInTopLevelObjects, resolveMethodsReferencesForEOProgram }
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.inlineorlines.ops._
import org.polystat.odin.parser.scala_parser_combinators.Parser
import org.polystat.odin.parser.scala_parser_combinators.errors.{ LexerError, ParserError }
import programs.mutualRecursionExample

import scala.io.Source
import scala.util.chaining._

object Sandbox extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    exitCode <- IO.pure(ExitCode.Success)
    //    mutualRecEORepr: String = mutualRecursionExample.toEO.allLinesToString
    //    _ <- IO(mutualRecEORepr.tap(println))

    fileName = "mutual_rec_example.eo"
    fileSourceResource = Resource.make(IO(Source.fromResource(fileName)))(src => IO(src.close()))
    fileContents <- fileSourceResource.use(src => IO(src.getLines().toVector.mkString("\n")))
    program <- IO.fromEither(Parser(fileContents).leftMap {
      case LexerError(msg) => new IllegalArgumentException(msg)
      case ParserError(msg) => new IllegalArgumentException(msg)
    })
    programText = program.toEO.allLinesToString
    _ <- IO(programText.tap(println))

    topLevelObjects <- resolveMethodsReferencesForEOProgram[IO](mutualRecursionExample)

    mutualRec <- findMutualRecursionInTopLevelObjects(topLevelObjects)
    mutualRecFiltered = mutualRec.filter(_.nonEmpty)

    _ <- IO.delay(println())
    _ <- IO.delay(
      for {
        mutualRecDep <- mutualRecFiltered
        (method, depChains) <- mutualRecDep.toVector
        depChain <- depChains.toVector
      } yield for {
        mutualRecMeth <- depChain.lastOption
      } yield {
        val mutualRecString =
          s"Method `${method.parentObject.objName}.${method.name}` " ++
            s"is mutually recursive with method " ++
            s"`${mutualRecMeth.parentObject.objName}.${mutualRecMeth.name}`"

        val dependencyChainString = depChain.append(method).map(m => s"${m.parentObject.objName}.${m.name}").mkString_(" -> ")

        println(mutualRecString ++ " through the following possible code path:\n" ++ dependencyChainString)
      }
    )
  } yield exitCode
}
