package org.polystat.odin.analysis

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import cats.syntax.functor._
import org.polystat.odin.utils.files
import org.scalacheck.{Prop, Test}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.genProgram
import org.polystat.odin.analysis.mutualrec.advanced._
import pprint.pprintln
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.scalatest.Assertion
import fs2.io.file.Files

import scala.util.Try

class MutualrecTests extends AnyWordSpec with Checkers {

  val params: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(1000)
    .withWorkers(4)

  def odinErrors(code: String): List[EOOdinAnalyzer.OdinAnalysisError] =
    EOOdinAnalyzer
      .analyzeSourceCode[String, IO](
        EOOdinAnalyzer.advancedMutualRecursionAnalyzer[IO]
      )(code)(sourceCodeEoParser())
      .compile
      .toList
      .unsafeRunSync()

  "odin" should {
    "find mutual recursion in auto-generated tests" in {
      val prop = Prop
        .forAllNoShrink(
          genProgram(15).retryUntil(p => p.findMultiObjectCycles.nonEmpty)
        ) { prog =>
          val code = prog.toEO + "\n"
          Try(if (odinErrors(code).isEmpty) pprintln(prog, height = 10000))
            .recover(_ => pprintln(prog, height = 10000))
          odinErrors(code).nonEmpty
        }
      check(prop, params)
    }

    def runTestsFrom[F[_]: Sync: Files](
      path: String,
      check: String => Assertion,
    ): F[Unit] =
      for {
        files <- files.readEoCodeFromResources[F](path)
      } yield files.foreach { case (name, code) =>
        registerTest(name)(check(code))
      }

    "manual tests" should {

      "pass" should {
        runTestsFrom[IO](
          "/mutualrec/with_recursion",
          code => {
            assert(odinErrors(code).nonEmpty)
          }
        ).unsafeRunSync()
      }

      "fail" should {
        runTestsFrom[IO](
          "/mutualrec/no_recursion",
          code => {
            assert(odinErrors(code).isEmpty)
          }
        ).unsafeRunSync()
      }

      "crash" should {
        runTestsFrom[IO](
          "/mutualrec/failing",
          code => {
            assertThrows[java.lang.Exception](odinErrors(code))
          }
        ).unsafeRunSync()
      }

    }

  }

}

object MutualrecTests {

  def main(args: Array[String]): Unit = {
    import org.polystat.odin.parser.eo.Parser

    val prog = List(
      Object(
        name = ObjectName(parent = None, name = "l"),
        parent = None,
        nestedObjs = List(
          Object(
            name = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "l")),
              name = "e"
            ),
            parent = None,
            nestedObjs = List(),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "e"
                ),
                name = "b"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "l")),
                    name = "e"
                  ),
                  name = "o"
                )
              ),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "e"
                ),
                name = "j"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "e"
                ),
                name = "o"
              ) -> Set()
            )
          ),
          Object(
            name = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "l")),
              name = "v"
            ),
            parent = None,
            nestedObjs = List(),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "v"
                ),
                name = "e"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "v"
                ),
                name = "z"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "l")),
                    name = "v"
                  ),
                  name = "e"
                )
              ),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "v"
                ),
                name = "y"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "l")),
                    name = "v"
                  ),
                  name = "e"
                )
              )
            )
          ),
          Object(
            name = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "l")),
              name = "i"
            ),
            parent = None,
            nestedObjs = List(),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "i"
                ),
                name = "f"
              ) -> Set()
            )
          )
        ),
        callGraph = Map(
          MethodName(
            whereDefined = ObjectName(parent = None, name = "l"),
            name = "w"
          ) -> Set(
            MethodName(
              whereDefined = ObjectName(parent = None, name = "l"),
              name = "c"
            )
          ),
          MethodName(
            whereDefined = ObjectName(parent = None, name = "l"),
            name = "k"
          ) -> Set(),
          MethodName(
            whereDefined = ObjectName(parent = None, name = "l"),
            name = "c"
          ) -> Set(
            MethodName(
              whereDefined = ObjectName(parent = None, name = "l"),
              name = "k"
            )
          )
        )
      ),
      Object(
        name = ObjectName(parent = None, name = "n"),
        parent = None,
        nestedObjs = List(),
        callGraph = Map(
          MethodName(
            whereDefined = ObjectName(parent = None, name = "n"),
            name = "c"
          ) -> Set(
            MethodName(
              whereDefined = ObjectName(parent = None, name = "n"),
              name = "q"
            )
          ),
          MethodName(
            whereDefined = ObjectName(parent = None, name = "n"),
            name = "q"
          ) -> Set()
        )
      ),
      Object(
        name = ObjectName(parent = None, name = "a"),
        parent = Some(
          value = ParentInfo(
            name = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "l")),
              name = "v"
            ),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "v"
                ),
                name = "e"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "v"
                ),
                name = "z"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "l")),
                    name = "v"
                  ),
                  name = "e"
                )
              ),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "l")),
                  name = "v"
                ),
                name = "y"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "l")),
                    name = "v"
                  ),
                  name = "e"
                )
              )
            ),
            parent = None
          )
        ),
        nestedObjs = List(
          Object(
            name = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "a")),
              name = "g"
            ),
            parent = None,
            nestedObjs = List(),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "a")),
                  name = "g"
                ),
                name = "h"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "a")),
                  name = "g"
                ),
                name = "q"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "a")),
                    name = "g"
                  ),
                  name = "l"
                )
              ),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "a")),
                  name = "g"
                ),
                name = "k"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "a")),
                    name = "g"
                  ),
                  name = "h"
                )
              ),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "a")),
                  name = "g"
                ),
                name = "l"
              ) -> Set()
            )
          ),
          Object(
            name = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "a")),
              name = "q"
            ),
            parent = None,
            nestedObjs = List(),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "a")),
                  name = "q"
                ),
                name = "e"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "a")),
                  name = "q"
                ),
                name = "h"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent =
                      Some(value = ObjectName(parent = None, name = "a")),
                    name = "q"
                  ),
                  name = "e"
                )
              )
            )
          )
        ),
        callGraph = Map(
          MethodName(
            whereDefined = ObjectName(
              parent = Some(value = ObjectName(parent = None, name = "l")),
              name = "v"
            ),
            name = "z"
          ) -> Set(
            MethodName(
              whereDefined = ObjectName(parent = None, name = "a"),
              name = "e"
            )
          ),
          MethodName(
            whereDefined = ObjectName(parent = None, name = "a"),
            name = "a"
          ) -> Set(),
          MethodName(
            whereDefined = ObjectName(parent = None, name = "a"),
            name = "e"
          ) -> Set(
            MethodName(
              whereDefined = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "l")),
                name = "v"
              ),
              name = "z"
            )
          ),
          MethodName(
            whereDefined = ObjectName(parent = None, name = "a"),
            name = "y"
          ) -> Set(
            MethodName(
              whereDefined = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "l")),
                name = "v"
              ),
              name = "z"
            )
          )
        )
      )
    )
    def showCallGraph(obj: Object): String =
      s"""Callgraph for ${obj.name.show}:
         |${obj.callGraph.show}
         |""".stripMargin

    def showCallGraphs(obj: Object): String =
      s"""${showCallGraph(obj)}
         |${obj.nestedObjs.map(showCallGraph).mkString("\n")}
         |""".stripMargin

    //    pprintln(prog, height=10000)

    val code = prog.toEO + "\n"
//      """
//        |+package sandbox.mutualrec
//        |+alias stdout org.eolang.io.stdout
//        |+alias sprintf org.eolang.txt.sprintf
//        |
//        |# Analysis should be able to detect mutual recursion
//        |# no matter where the base and derived classes are.
//        |
//        |# [abstractions.]base.f calls g
//        |# [implementations.]derived extends abstractions.base
// |# derived.g overrides base g to call derived.f (which is inherited from
    // base)
//        |# So, we have a loop:
//        |# derived.g -> derived.f. -> derived.g
//        |
//        |[] > nested_objects
//        |  [] > abstractions
//        |    [] > base
//        |      [self a] > f
//        |        self.g self a > @
//        |      [self a] > g
//        |        a > @
//        |  [] > implementations
//        |    [] > derived
//        |      nested_objects.abstractions.base > @
//        |      [self a] > g
//        |        self.f self a > @
//        |
//        |""".stripMargin
    println(code)
    println(prog.map(showCallGraphs).mkString("\n"))
    println("______________________________")
    println(prog.findMultiObjectCycles.map(_.show).mkString("\n"))
//    println(
//      EOOdinAnalyzer
//        .analyzeSourceCode[String, IO](
//          EOOdinAnalyzer.advancedMutualRecursionAnalyzer
//        )(code)(sourceCodeEoParser())
//        .compile
//        .toList
//        .unsafeRunSync()
//    )

    Parser
      .parse(code)
      .flatMap(ast => {
        val tree = Analyzer.buildTree[Either[String, *]](ast)
        val prog = tree.flatMap(Analyzer.buildProgram[Either[String, *]])
        val errors = prog.map(Analyzer.findErrors)
        println(errors)
        prog.map(_.toEO).foreach(println)
        prog.map(_.map(showCallGraphs).mkString("\n")).foreach(println)
        prog

      })
      .foreach(p => pprintln(p, height = 10000))

//    println(Parser.parse("#START\n" + aboba.toEO + "#END\n"))

  }

}
