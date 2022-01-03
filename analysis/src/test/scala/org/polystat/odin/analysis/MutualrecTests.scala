package org.polystat.odin.analysis

import cats.effect.unsafe.implicits.global
import cats.effect.IO
import org.polystat.odin.utils.files
import org.scalacheck.{Prop, Test}
import org.scalatest.wordspec.AnyWordSpec
import org.scalatestplus.scalacheck.Checkers
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.analysis.gens.MutualRecursionTestGen.genProgram
import org.polystat.odin.analysis.mutualrec.advanced._
import pprint.pprintln
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._

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
          genProgram(10).retryUntil(p =>
            if (p.findMultiObjectCycles.nonEmpty) {
//              println(p.findMultiObjectCycles)
              p.findMultiObjectCycles.nonEmpty
            } else p.findMultiObjectCycles.nonEmpty
          )
        ) { obj =>
          val code = obj.toEO + "\n"
          try {
            if (odinErrors(code).isEmpty) pprintln(obj, height = 10000)
          } catch {
            case _: Exception => pprintln(obj, height = 10000)
          }
          odinErrors(code).nonEmpty
        }
      check(prop, params)
    }

    "manual tests" should {
      (for {
        files <-
          files.readEoCodeFromResources[IO](
            "/mutualrec"
          )
      } yield files.foreach { case (name, code) =>
        registerIgnoredTest(name) {
          val producedErrors = odinErrors(code)
          println(producedErrors)
          assert(producedErrors.nonEmpty)
        }
      }).unsafeRunSync()
    }

  }

}

object MutualrecTests {

  def main(args: Array[String]): Unit = {
    import org.polystat.odin.parser.eo.Parser
    val prog = Program(
      objs = List(
        Object(
          name = ObjectName(parent = None, name = "b"),
          parent = None,
          nestedObjs = List(),
          callGraph = Map(
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "n") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x") -> Set()
          )
        ),
        Object(
          name = ObjectName(parent = None, name = "g"),
          parent = Some(
            value = ParentInfo(
              name = ObjectName(parent = None, name = "b"),
              callGraph = Map(
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w") -> Set(),
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "n") -> Set(
                  MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x")
                ),
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d") -> Set(
                  MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w")
                ),
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x") -> Set()
              ),
              parent = None
            )
          ),
          nestedObjs = List(),
          callGraph = Map(
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "k") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "n")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "n") -> Set()
          )
        ),
        Object(
          name = ObjectName(parent = None, name = "k"),
          parent = Some(
            value = ParentInfo(
              name = ObjectName(parent = None, name = "g"),
              callGraph = Map(
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w") -> Set(),
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d") -> Set(
                  MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w")
                ),
                MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "k") -> Set(
                  MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "n")
                ),
                MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x") -> Set(),
                MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "n") -> Set()
              ),
              parent = Some(
                value = ParentInfo(
                  name = ObjectName(parent = None, name = "b"),
                  callGraph = Map(
                    MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w") -> Set(),
                    MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "n") -> Set(
                      MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x")
                    ),
                    MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d") -> Set(
                      MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "w")
                    ),
                    MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "x") -> Set()
                  ),
                  parent = None
                )
              )
            )
          ),
          nestedObjs = List(),
          callGraph = Map(
            MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "u") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "g") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "w")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "x") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "g"), name = "k") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "n")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "n") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "k"), name = "w") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "b"), name = "d")
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

    val code = "#START\n" + prog.toEO + "#END\n"
    println(code)
    println(prog.objs.map(showCallGraphs).mkString("\n"))
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
        val tree = Analyzer.buildTree(ast)
        val prog = tree.flatMap(Analyzer.buildProgram)
        val errors = prog.map(Analyzer.findErrors)
        println(errors)
        prog.map(_.toEO).foreach(println)
        prog.map(_.objs.map(showCallGraphs).mkString("\n")).foreach(println)
        prog

      })
      .foreach(_ =>
//        pprintln(p, height=10000)
        ()
      )

//    println(Parser.parse("#START\n" + aboba.toEO + "#END\n"))

  }

}
