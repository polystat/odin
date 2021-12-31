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
          genProgram(2).retryUntil(p =>
            if (p.findMultiObjectCycles.nonEmpty) {
              println(p.findMultiObjectCycles)
              p.findMultiObjectCycles.nonEmpty
            } else p.findMultiObjectCycles.nonEmpty
          )
        ) { obj =>
          val code = obj.toEO + "\n"
          if (odinErrors(code).isEmpty) pprintln(obj, height = 10000)
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
    val prog =
      Program(
        objs = List(
          Object(
            name = ObjectName(parent = None, name = "n"),
            parent = None,
            nestedObjs = List(
              Object(
                name = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "n")),
                  name = "j"
                ),
                parent = None,
                nestedObjs = List(
                  Object(
                    name = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "j"
                        )
                      ),
                      name = "y"
                    ),
                    parent = None,
                    nestedObjs = List(
                      Object(
                        name = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "r"
                        ),
                        parent = None,
                        nestedObjs = List(),
                        callGraph = Map(
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(
                                    value = ObjectName(
                                      parent = Some(value =
                                        ObjectName(parent = None, name = "n")
                                      ),
                                      name = "j"
                                    )
                                  ),
                                  name = "y"
                                )
                              ),
                              name = "r"
                            ),
                            name = "v"
                          ) -> Set(),
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(
                                    value = ObjectName(
                                      parent = Some(value =
                                        ObjectName(parent = None, name = "n")
                                      ),
                                      name = "j"
                                    )
                                  ),
                                  name = "y"
                                )
                              ),
                              name = "r"
                            ),
                            name = "s"
                          ) -> Set(
                            MethodName(
                              whereDefined = ObjectName(
                                parent = Some(
                                  value = ObjectName(
                                    parent = Some(
                                      value = ObjectName(
                                        parent = Some(value =
                                          ObjectName(parent = None, name = "n")
                                        ),
                                        name = "j"
                                      )
                                    ),
                                    name = "y"
                                  )
                                ),
                                name = "r"
                              ),
                              name = "v"
                            )
                          ),
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(
                                    value = ObjectName(
                                      parent = Some(value =
                                        ObjectName(parent = None, name = "n")
                                      ),
                                      name = "j"
                                    )
                                  ),
                                  name = "y"
                                )
                              ),
                              name = "r"
                            ),
                            name = "j"
                          ) -> Set(),
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(
                                    value = ObjectName(
                                      parent = Some(value =
                                        ObjectName(parent = None, name = "n")
                                      ),
                                      name = "j"
                                    )
                                  ),
                                  name = "y"
                                )
                              ),
                              name = "r"
                            ),
                            name = "c"
                          ) -> Set()
                        )
                      ),
                      Object(
                        name = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "x"
                        ),
                        parent = None,
                        nestedObjs = List(),
                        callGraph = Map(
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(
                                    value = ObjectName(
                                      parent = Some(value =
                                        ObjectName(parent = None, name = "n")
                                      ),
                                      name = "j"
                                    )
                                  ),
                                  name = "y"
                                )
                              ),
                              name = "x"
                            ),
                            name = "u"
                          ) -> Set(),
                          MethodName(
                            whereDefined = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(
                                    value = ObjectName(
                                      parent = Some(value =
                                        ObjectName(parent = None, name = "n")
                                      ),
                                      name = "j"
                                    )
                                  ),
                                  name = "y"
                                )
                              ),
                              name = "x"
                            ),
                            name = "q"
                          ) -> Set()
                        )
                      )
                    ),
                    callGraph = Map(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        ),
                        name = "u"
                      ) -> Set(
                        MethodName(
                          whereDefined = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(value =
                                  ObjectName(parent = None, name = "n")
                                ),
                                name = "j"
                              )
                            ),
                            name = "y"
                          ),
                          name = "j"
                        )
                      ),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        ),
                        name = "j"
                      ) -> Set()
                    )
                  )
                ),
                callGraph = Map(
                  MethodName(
                    whereDefined = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "j"
                    ),
                    name = "a"
                  ) -> Set(),
                  MethodName(
                    whereDefined = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "j"
                    ),
                    name = "i"
                  ) -> Set(
                    MethodName(
                      whereDefined = ObjectName(
                        parent =
                          Some(value = ObjectName(parent = None, name = "n")),
                        name = "j"
                      ),
                      name = "g"
                    )
                  ),
                  MethodName(
                    whereDefined = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "j"
                    ),
                    name = "g"
                  ) -> Set()
                )
              ),
              Object(
                name = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "n")),
                  name = "l"
                ),
                parent = None,
                nestedObjs = List(),
                callGraph = Map(
                  MethodName(
                    whereDefined = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "l"
                    ),
                    name = "q"
                  ) -> Set(),
                  MethodName(
                    whereDefined = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "l"
                    ),
                    name = "v"
                  ) -> Set()
                )
              ),
              Object(
                name = ObjectName(
                  parent = Some(value = ObjectName(parent = None, name = "n")),
                  name = "a"
                ),
                parent = None,
                nestedObjs = List(
                  Object(
                    name = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "a"
                        )
                      ),
                      name = "u"
                    ),
                    parent = None,
                    nestedObjs = List(),
                    callGraph = Map(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "u"
                        ),
                        name = "b"
                      ) -> Set(
                        MethodName(
                          whereDefined = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(value =
                                  ObjectName(parent = None, name = "n")
                                ),
                                name = "a"
                              )
                            ),
                            name = "u"
                          ),
                          name = "h"
                        )
                      ),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "u"
                        ),
                        name = "h"
                      ) -> Set()
                    )
                  ),
                  Object(
                    name = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "a"
                        )
                      ),
                      name = "h"
                    ),
                    parent = None,
                    nestedObjs = List(),
                    callGraph = Map(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "h"
                        ),
                        name = "q"
                      ) -> Set()
                    )
                  ),
                  Object(
                    name = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "a"
                        )
                      ),
                      name = "v"
                    ),
                    parent = None,
                    nestedObjs = List(),
                    callGraph = Map(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "v"
                        ),
                        name = "y"
                      ) -> Set(),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "v"
                        ),
                        name = "v"
                      ) -> Set(),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "v"
                        ),
                        name = "h"
                      ) -> Set(),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "a"
                            )
                          ),
                          name = "v"
                        ),
                        name = "m"
                      ) -> Set()
                    )
                  )
                ),
                callGraph = Map(
                  MethodName(
                    whereDefined = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "a"
                    ),
                    name = "o"
                  ) -> Set()
                )
              )
            ),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(parent = None, name = "n"),
                name = "u"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(parent = None, name = "n"),
                name = "d"
              ) -> Set(),
              MethodName(
                whereDefined = ObjectName(parent = None, name = "n"),
                name = "c"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(parent = None, name = "n"),
                  name = "u"
                )
              )
            )
          ),
          Object(
            name = ObjectName(parent = None, name = "y"),
            parent = Some(
              value = Object(
                name = ObjectName(
                  parent = Some(
                    value = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "j"
                    )
                  ),
                  name = "y"
                ),
                parent = None,
                nestedObjs = List(
                  Object(
                    name = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "r"
                    ),
                    parent = None,
                    nestedObjs = List(),
                    callGraph = Map(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "r"
                        ),
                        name = "v"
                      ) -> Set(),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "r"
                        ),
                        name = "s"
                      ) -> Set(
                        MethodName(
                          whereDefined = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(
                                  value = ObjectName(
                                    parent = Some(value =
                                      ObjectName(parent = None, name = "n")
                                    ),
                                    name = "j"
                                  )
                                ),
                                name = "y"
                              )
                            ),
                            name = "r"
                          ),
                          name = "v"
                        )
                      ),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "r"
                        ),
                        name = "j"
                      ) -> Set(),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "r"
                        ),
                        name = "c"
                      ) -> Set()
                    )
                  ),
                  Object(
                    name = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "x"
                    ),
                    parent = None,
                    nestedObjs = List(),
                    callGraph = Map(
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "x"
                        ),
                        name = "u"
                      ) -> Set(),
                      MethodName(
                        whereDefined = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(
                                value = ObjectName(
                                  parent = Some(value =
                                    ObjectName(parent = None, name = "n")
                                  ),
                                  name = "j"
                                )
                              ),
                              name = "y"
                            )
                          ),
                          name = "x"
                        ),
                        name = "q"
                      ) -> Set()
                    )
                  )
                ),
                callGraph = Map(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "j"
                        )
                      ),
                      name = "y"
                    ),
                    name = "u"
                  ) -> Set(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(value =
                              ObjectName(parent = None, name = "n")
                            ),
                            name = "j"
                          )
                        ),
                        name = "y"
                      ),
                      name = "j"
                    )
                  ),
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "j"
                        )
                      ),
                      name = "y"
                    ),
                    name = "j"
                  ) -> Set()
                )
              )
            ),
            nestedObjs = List(
              Object(
                name = ObjectName(
                  parent = Some(
                    value = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "j"
                        )
                      ),
                      name = "y"
                    )
                  ),
                  name = "r"
                ),
                parent = None,
                nestedObjs = List(),
                callGraph = Map(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "r"
                    ),
                    name = "v"
                  ) -> Set(),
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "r"
                    ),
                    name = "s"
                  ) -> Set(
                    MethodName(
                      whereDefined = ObjectName(
                        parent = Some(
                          value = ObjectName(
                            parent = Some(
                              value = ObjectName(
                                parent = Some(value =
                                  ObjectName(parent = None, name = "n")
                                ),
                                name = "j"
                              )
                            ),
                            name = "y"
                          )
                        ),
                        name = "r"
                      ),
                      name = "v"
                    )
                  ),
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "r"
                    ),
                    name = "j"
                  ) -> Set(),
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "r"
                    ),
                    name = "c"
                  ) -> Set()
                )
              ),
              Object(
                name = ObjectName(
                  parent = Some(
                    value = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent =
                            Some(value = ObjectName(parent = None, name = "n")),
                          name = "j"
                        )
                      ),
                      name = "y"
                    )
                  ),
                  name = "x"
                ),
                parent = None,
                nestedObjs = List(),
                callGraph = Map(
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "x"
                    ),
                    name = "u"
                  ) -> Set(),
                  MethodName(
                    whereDefined = ObjectName(
                      parent = Some(
                        value = ObjectName(
                          parent = Some(
                            value = ObjectName(
                              parent = Some(value =
                                ObjectName(parent = None, name = "n")
                              ),
                              name = "j"
                            )
                          ),
                          name = "y"
                        )
                      ),
                      name = "x"
                    ),
                    name = "q"
                  ) -> Set()
                )
              )
            ),
            callGraph = Map(
              MethodName(
                whereDefined = ObjectName(
                  parent = Some(
                    value = ObjectName(
                      parent =
                        Some(value = ObjectName(parent = None, name = "n")),
                      name = "j"
                    )
                  ),
                  name = "y"
                ),
                name = "u"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(parent = None, name = "y"),
                  name = "j"
                )
              ),
              MethodName(
                whereDefined = ObjectName(parent = None, name = "y"),
                name = "e"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(parent = None, name = "y"),
                  name = "j"
                )
              ),
              MethodName(
                whereDefined = ObjectName(parent = None, name = "y"),
                name = "j"
              ) -> Set(
                MethodName(
                  whereDefined = ObjectName(
                    parent = Some(
                      value = ObjectName(
                        parent =
                          Some(value = ObjectName(parent = None, name = "n")),
                        name = "j"
                      )
                    ),
                    name = "y"
                  ),
                  name = "u"
                )
              )
            )
          )
        )
      )

    pprintln(prog, height=10000)

    val code = "#START\n" + prog.toEO + "#END\n"
    println(code)
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
        val prog = Analyzer.buildTree(ast).flatMap(Analyzer.buildProgram)
        prog

      }).foreach(p => pprintln(p, height=10000))

//    println(Parser.parse("#START\n" + aboba.toEO + "#END\n"))

  }

}
