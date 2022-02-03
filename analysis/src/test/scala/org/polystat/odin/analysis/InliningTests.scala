package org.polystat.odin.analysis

import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Context
import org.scalatest.wordspec.AnyWordSpec
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.ToEO.instances._

class InliningTests extends AnyWordSpec {

  case class LocatorTestCase(
    label: String,
    codeBefore: EOProg[EOExprOnly],
    codeAfter: EOProg[EOExprOnly],
  )

  "setLocators" should {
    val locatorTests: List[LocatorTestCase] = List(
      LocatorTestCase(
        label = "arbitrary",
        codeBefore = EOProg(
          metas = EOMetas(pack = None, metas = Vector()),
          bnds = Vector(
            EOBndExpr(
              bndName = EOAnyNameBnd(name = LazyName(name = "outer")),
              expr = Fix(
                EOObj(
                  freeAttrs = Vector(),
                  varargAttr = None,
                  bndAttrs = Vector(
                    EOBndExpr(
                      bndName = EOAnyNameBnd(name = LazyName(name = "self")),
                      expr = Fix(
                        EOObj(
                          freeAttrs = Vector(),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              bndName =
                                EOAnyNameBnd(name = LazyName(name = "magic")),
                              expr = Fix[EOExpr](EOIntData(int = 256))
                            ),
                            EOBndExpr(
                              bndName =
                                EOAnyNameBnd(name = LazyName(name = "dummy")),
                              expr = Fix(
                                EOObj(
                                  freeAttrs = Vector(),
                                  varargAttr = None,
                                  bndAttrs = Vector(
                                    EOBndExpr(
                                      bndName = EOAnyNameBnd(name =
                                        LazyName(name = "aboba")
                                      ),
                                      expr = Fix(
                                        EOObj(
                                          freeAttrs =
                                            Vector(LazyName(name = "self")),
                                          varargAttr = None,
                                          bndAttrs = Vector(
                                            EOBndExpr(
                                              bndName = EODecoration,
                                              expr =
                                                Fix[EOExpr](EOIntData(int = 22))
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    EOBndExpr(
                                      bndName = EOAnyNameBnd(name =
                                        LazyName(name = "innerMethod")
                                      ),
                                      expr = Fix(
                                        EOObj(
                                          freeAttrs = Vector(
                                            LazyName(name = "self"),
                                            LazyName(name = "outer")
                                          ),
                                          varargAttr = None,
                                          bndAttrs = Vector(
                                            EOBndExpr(
                                              bndName = EODecoration,
                                              expr = Fix(
                                                EOCopy(
                                                  trg = Fix(
                                                    EODot(
                                                      src = Fix[EOExpr](
                                                        EOSimpleApp(name =
                                                          "self"
                                                        )
                                                      ),
                                                      name = "aboba"
                                                    )
                                                  ),
                                                  args = NonEmpty[Vector[
                                                    EOBnd[EOExprOnly]
                                                  ]](
                                                    EOAnonExpr(
                                                      Fix[EOExpr](
                                                        EOSimpleApp("self")
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    EOBndExpr(
                                      bndName = EODecoration,
                                      expr = Fix(
                                        EOCopy(
                                          trg = Fix(
                                            EODot(
                                              src = Fix[EOExpr](
                                                EOSimpleApp(name = "self")
                                              ),
                                              name = "innerMethod"
                                            )
                                          ),
                                          args =
                                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                              EOAnonExpr(
                                                Fix[EOExpr](EOSimpleApp("self"))
                                              ),
                                              EOAnonExpr(
                                                Fix[EOExpr](EOSimpleApp("self"))
                                              )
                                            )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            EOBndExpr(
                              bndName = EODecoration,
                              expr = Fix(
                                EOCopy(
                                  trg = Fix[EOExpr](EOSimpleApp(name = "self")),
                                  args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                    EOAnonExpr(Fix[EOExpr](EOStrData("yahoo")))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    EOBndExpr(
                      bndName = EOAnyNameBnd(name = LazyName(name = "method")),
                      expr = Fix(
                        EOObj(
                          freeAttrs = Vector(LazyName(name = "self")),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              bndName = EODecoration,
                              expr = Fix(
                                EODot(
                                  src = Fix[EOExpr](EOSimpleApp(name = "self")),
                                  name = "magic"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        ),
        codeAfter = EOProg(
          metas = EOMetas(pack = None, metas = Vector()),
          bnds = Vector(
            EOBndExpr(
              bndName = EOAnyNameBnd(name = LazyName(name = "outer")),
              expr = Fix(
                EOObj(
                  freeAttrs = Vector(),
                  varargAttr = None,
                  bndAttrs = Vector(
                    EOBndExpr(
                      bndName = EOAnyNameBnd(name = LazyName(name = "self")),
                      expr = Fix(
                        EOObj(
                          freeAttrs = Vector(),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              bndName =
                                EOAnyNameBnd(name = LazyName(name = "magic")),
                              expr = Fix[EOExpr](EOIntData(int = 256))
                            ),
                            EOBndExpr(
                              bndName =
                                EOAnyNameBnd(name = LazyName(name = "dummy")),
                              expr = Fix(
                                EOObj(
                                  freeAttrs = Vector(),
                                  varargAttr = None,
                                  bndAttrs = Vector(
                                    EOBndExpr(
                                      bndName = EOAnyNameBnd(name =
                                        LazyName(name = "aboba")
                                      ),
                                      expr = Fix(
                                        EOObj(
                                          freeAttrs =
                                            Vector(LazyName(name = "self")),
                                          varargAttr = None,
                                          bndAttrs = Vector(
                                            EOBndExpr(
                                              bndName = EODecoration,
                                              expr =
                                                Fix[EOExpr](EOIntData(int = 22))
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    EOBndExpr(
                                      bndName = EOAnyNameBnd(name =
                                        LazyName(name = "innerMethod")
                                      ),
                                      expr = Fix(
                                        EOObj(
                                          freeAttrs = Vector(
                                            LazyName(name = "self"),
                                            LazyName(name = "outer")
                                          ),
                                          varargAttr = None,
                                          bndAttrs = Vector(
                                            EOBndExpr(
                                              bndName = EODecoration,
                                              expr = Fix(
                                                EOCopy(
                                                  trg = Fix(
                                                    EODot(
                                                      src = Fix[EOExpr](
                                                        EOSimpleAppWithLocator(
                                                          name =
                                                            "self",
                                                          0
                                                        )
                                                      ),
                                                      name = "aboba"
                                                    )
                                                  ),
                                                  args = NonEmpty[Vector[
                                                    EOBnd[EOExprOnly]
                                                  ]](
                                                    EOAnonExpr(
                                                      Fix[EOExpr](
                                                        EOSimpleAppWithLocator(
                                                          "self",
                                                          0
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ),
                                    EOBndExpr(
                                      bndName = EODecoration,
                                      expr = Fix(
                                        EOCopy(
                                          trg = Fix(
                                            EODot(
                                              src = Fix[EOExpr](
                                                EOSimpleAppWithLocator(
                                                  name = "self",
                                                  2
                                                )
                                              ),
                                              name = "innerMethod"
                                            )
                                          ),
                                          args =
                                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                              EOAnonExpr(
                                                Fix[EOExpr](
                                                  EOSimpleAppWithLocator(
                                                    name = "self",
                                                    2
                                                  )
                                                )
                                              ),
                                              EOAnonExpr(
                                                Fix[EOExpr](
                                                  EOSimpleAppWithLocator(
                                                    name = "self",
                                                    2
                                                  )
                                                )
                                              )
                                            )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ),
                            EOBndExpr(
                              bndName = EODecoration,
                              expr = Fix(
                                EOCopy(
                                  trg = Fix[EOExpr](
                                    EOSimpleAppWithLocator(name = "self", 1)
                                  ),
                                  args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                    EOAnonExpr(Fix[EOExpr](EOStrData("yahoo")))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    EOBndExpr(
                      bndName = EOAnyNameBnd(name = LazyName(name = "method")),
                      expr = Fix(
                        EOObj(
                          freeAttrs = Vector(LazyName(name = "self")),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              bndName = EODecoration,
                              expr = Fix(
                                EODot(
                                  src = Fix[EOExpr](
                                    EOSimpleAppWithLocator(name = "self", 0)
                                  ),
                                  name = "magic"
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      )
    )
    "set locators correctly" should {
      locatorTests.foreach { case LocatorTestCase(label, before, after) =>
        registerTest(label) {
          val expected: EOProg[EOExprOnly] = after
          val obtained: EOProg[EOExprOnly] = Context.setLocators(before)
          println("Expected: ")
          println(expected.toEOPretty)
          println("Obtained: ")
          println(obtained.toEOPretty)
          assertResult(expected)(obtained)

        }

      }
    }
  }

}

object InliningTests {

  def main(args: Array[String]): Unit = {
    val code = """[] > outer
                 |  [] > self
                 |    256 > magic
                 |    [] > dummy
                 |      [self] > aboba
                 |        22 > @
                 |      [self outer] > innerMethod
                 |        self.aboba self > @
                 |      self.innerMethod self self > @
                 |    self "yahoo" > @
                 |  [self] > method
                 |    self.magic > @
                 |""".stripMargin

    Parser.parse(code) match {
      case Left(value) => println(value)
      case Right(value) => pprint.pprintln(value)
    }
  }

}
