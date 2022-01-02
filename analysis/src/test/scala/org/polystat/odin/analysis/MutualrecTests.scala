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
    .withMinSuccessfulTests(10000)

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
          genProgram(4).retryUntil(p =>
            if (p.findMultiObjectCycles.nonEmpty) {
              println(p.findMultiObjectCycles)
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
          name = ObjectName(parent = None, name = "u"),
          parent = None,
          nestedObjs = List(
            Object(
              name = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "u")),
                name = "f"
              ),
              parent = None,
              nestedObjs = List(
                Object(
                  name = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      )
                    ),
                    name = "n"
                  ),
                  parent = None,
                  nestedObjs = List(),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "f"
                          )
                        ),
                        name = "n"
                      ),
                      name = "b"
                    ) -> Set()
                  )
                )
              ),
              callGraph = Map(
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  name = "l"
                ) -> Set(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(value = ObjectName(parent = None, name = "u")),
                      name = "f"
                    ),
                    name = "c"
                  )
                ),
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  name = "c"
                ) -> Set(),
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  name = "p"
                ) -> Set()
              )
            ),
            Object(
              name = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "u")),
                name = "m"
              ),
              parent = None,
              nestedObjs = List(),
              callGraph = Map(
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "m"
                  ),
                  name = "u"
                ) -> Set()
              )
            ),
            Object(
              name = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "u")),
                name = "y"
              ),
              parent = None,
              nestedObjs = List(
                Object(
                  name = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "y"
                      )
                    ),
                    name = "y"
                  ),
                  parent = None,
                  nestedObjs = List(),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "y"
                          )
                        ),
                        name = "y"
                      ),
                      name = "e"
                    ) -> Set(),
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "y"
                          )
                        ),
                        name = "y"
                      ),
                      name = "z"
                    ) -> Set(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value = ObjectName(parent = None, name = "u")),
                              name = "y"
                            )
                          ),
                          name = "y"
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
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "y"
                  ),
                  name = "m"
                ) -> Set()
              )
            )
          ),
          callGraph = Map(
            MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "j") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "a") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "z")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "z") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "r") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "a")
            )
          )
        ),
        Object(
          name = ObjectName(parent = None, name = "z"),
          parent = Some(
            value = Object(
              name = ObjectName(parent = None, name = "u"),
              parent = None,
              nestedObjs = List(
                Object(
                  name = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  parent = None,
                  nestedObjs = List(
                    Object(
                      name = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "f"
                          )
                        ),
                        name = "n"
                      ),
                      parent = None,
                      nestedObjs = List(),
                      callGraph = Map(
                        MethodName(
                          whereDefined = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(value = ObjectName(parent = None, name = "u")),
                                name = "f"
                              )
                            ),
                            name = "n"
                          ),
                          name = "b"
                        ) -> Set()
                      )
                    )
                  ),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      ),
                      name = "l"
                    ) -> Set(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(value = ObjectName(parent = None, name = "u")),
                          name = "f"
                        ),
                        name = "c"
                      )
                    ),
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      ),
                      name = "c"
                    ) -> Set(),
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      ),
                      name = "p"
                    ) -> Set()
                  )
                ),
                Object(
                  name = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "m"
                  ),
                  parent = None,
                  nestedObjs = List(),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "m"
                      ),
                      name = "u"
                    ) -> Set()
                  )
                ),
                Object(
                  name = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "y"
                  ),
                  parent = None,
                  nestedObjs = List(
                    Object(
                      name = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "y"
                          )
                        ),
                        name = "y"
                      ),
                      parent = None,
                      nestedObjs = List(),
                      callGraph = Map(
                        MethodName(
                          whereDefined = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(value = ObjectName(parent = None, name = "u")),
                                name = "y"
                              )
                            ),
                            name = "y"
                          ),
                          name = "e"
                        ) -> Set(),
                        MethodName(
                          whereDefined = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(value = ObjectName(parent = None, name = "u")),
                                name = "y"
                              )
                            ),
                            name = "y"
                          ),
                          name = "z"
                        ) -> Set(
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value = ObjectName(parent = None, name = "u")),
                                  name = "y"
                                )
                              ),
                              name = "y"
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
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "y"
                      ),
                      name = "m"
                    ) -> Set()
                  )
                )
              ),
              callGraph = Map(
                MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "j") -> Set(),
                MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "a") -> Set(
                  MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "z")
                ),
                MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "z") -> Set(),
                MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "r") -> Set(
                  MethodName(whereDefined = ObjectName(parent = None, name = "u"), name = "a")
                )
              )
            )
          ),
          nestedObjs = List(
            Object(
              name = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "u")),
                name = "f"
              ),
              parent = None,
              nestedObjs = List(
                Object(
                  name = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      )
                    ),
                    name = "n"
                  ),
                  parent = None,
                  nestedObjs = List(),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "f"
                          )
                        ),
                        name = "n"
                      ),
                      name = "b"
                    ) -> Set()
                  )
                ),
                Object(
                  name = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      )
                    ),
                    name = "i"
                  ),
                  parent = None,
                  nestedObjs = List(),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "f"
                          )
                        ),
                        name = "i"
                      ),
                      name = "d"
                    ) -> Set(),
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "f"
                          )
                        ),
                        name = "i"
                      ),
                      name = "a"
                    ) -> Set(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value = ObjectName(parent = None, name = "u")),
                              name = "f"
                            )
                          ),
                          name = "i"
                        ),
                        name = "d"
                      )
                    ),
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "f"
                          )
                        ),
                        name = "i"
                      ),
                      name = "y"
                    ) -> Set(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value = ObjectName(parent = None, name = "u")),
                              name = "f"
                            )
                          ),
                          name = "i"
                        ),
                        name = "a"
                      )
                    )
                  )
                )
              ),
              callGraph = Map(
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  name = "l"
                ) -> Set(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(value = ObjectName(parent = None, name = "u")),
                      name = "f"
                    ),
                    name = "c"
                  )
                ),
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  name = "c"
                ) -> Set(),
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  ),
                  name = "p"
                ) -> Set()
              )
            ),
            Object(
              name = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "u")),
                name = "m"
              ),
              parent = None,
              nestedObjs = List(),
              callGraph = Map(
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "m"
                  ),
                  name = "u"
                ) -> Set()
              )
            ),
            Object(
              name = ObjectName(
                parent = Some(value = ObjectName(parent = None, name = "u")),
                name = "y"
              ),
              parent = None,
              nestedObjs = List(
                Object(
                  name = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "y"
                      )
                    ),
                    name = "y"
                  ),
                  parent = None,
                  nestedObjs = List(),
                  callGraph = Map(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "y"
                          )
                        ),
                        name = "y"
                      ),
                      name = "e"
                    ) -> Set(),
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value = ObjectName(parent = None, name = "u")),
                            name = "y"
                          )
                        ),
                        name = "y"
                      ),
                      name = "z"
                    ) -> Set(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value = ObjectName(parent = None, name = "u")),
                              name = "y"
                            )
                          ),
                          name = "y"
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
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "y"
                  ),
                  name = "m"
                ) -> Set()
              )
            )
          ),
          callGraph = Map(
            MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "a") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "r") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "a")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "j") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "z") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "j")
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "u") -> Set(),
            MethodName(whereDefined = ObjectName(parent = None, name = "z"), name = "k") -> Set()
          )
        ),
        Object(
          name = ObjectName(parent = None, name = "e"),
          parent = Some(
            value = Object(
              name = ObjectName(
                parent = Some(
                  value = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  )
                ),
                name = "i"
              ),
              parent = None,
              nestedObjs = List(),
              callGraph = Map(
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      )
                    ),
                    name = "i"
                  ),
                  name = "d"
                ) -> Set(),
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      )
                    ),
                    name = "i"
                  ),
                  name = "a"
                ) -> Set(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(value = ObjectName(parent = None, name = "u")),
                          name = "f"
                        )
                      ),
                      name = "i"
                    ),
                    name = "d"
                  )
                ),
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent = Some(value = ObjectName(parent = None, name = "u")),
                        name = "f"
                      )
                    ),
                    name = "i"
                  ),
                  name = "y"
                ) -> Set(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(value = ObjectName(parent = None, name = "u")),
                          name = "f"
                        )
                      ),
                      name = "i"
                    ),
                    name = "a"
                  )
                )
              )
            )
          ),
          nestedObjs = List(),
          callGraph = Map(
            MethodName(
              whereDefined = ObjectName(
                parent = Some(
                  value = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  )
                ),
                name = "i"
              ),
              name = "d"
            ) -> Set(),
            MethodName(
              whereDefined = ObjectName(
                parent = Some(
                  value = ObjectName(
                    parent = Some(value = ObjectName(parent = None, name = "u")),
                    name = "f"
                  )
                ),
                name = "i"
              ),
              name = "y"
            ) -> Set(MethodName(whereDefined = ObjectName(parent = None, name = "e"), name = "a")),
            MethodName(whereDefined = ObjectName(parent = None, name = "e"), name = "g") -> Set(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(
                    value = ObjectName(
                      parent = Some(value = ObjectName(parent = None, name = "u")),
                      name = "f"
                    )
                  ),
                  name = "i"
                ),
                name = "y"
              )
            ),
            MethodName(whereDefined = ObjectName(parent = None, name = "e"), name = "a") -> Set(
              MethodName(whereDefined = ObjectName(parent = None, name = "e"), name = "g")
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
