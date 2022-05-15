package org.polystat.odin.parser.ast_tests

import cats.data.NonEmptyList
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.TestUtils.TestCase

object FullProgramExamples {

  val correct: List[TestCase[EOProg[EOExprOnly]]] = List(
    mutualRecursionExample,
    booleanLiteralsExample,
    dirWalk,
  )

  private lazy val dirWalk: TestCase[EOProg[EOExprOnly]] = TestCase(
    label = "dir walk",
    code =
      """(dir "/tmp").walk
        |  * ([f] (f.is-dir > @))
        |""".stripMargin,
    ast = Some(
      EOProg(
        metas = EOMetas(pack = None, metas = Vector()),
        bnds = Vector(
          EOAnonExpr(
            Fix(
              EOCopy(
                trg = Fix(
                  EODot(
                    src = Fix(
                      EOCopy(
                        trg = Fix(EOSimpleApp[EOExprOnly]("dir")),
                        NonEmpty[Vector[EOBnd[EOExprOnly]]](
                          EOAnonExpr(Fix(EOStrData[EOExprOnly]("/tmp")))
                        )
                      )
                    ),
                    name = "walk"
                  )
                ),
                args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                  EOAnonExpr(
                    Fix(
                      EOArray(
                        elems = Vector(
                          EOAnonExpr(
                            Fix(
                              EOObj(
                                freeAttrs = Vector(LazyName("f")),
                                varargAttr = None,
                                bndAttrs = Vector(
                                  EOBndExpr(
                                    bndName = EODecoration,
                                    expr = Fix(
                                      EODot(
                                        src = Fix(EOSimpleApp[EOExprOnly]("f")),
                                        name = "is-dir"
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
        )
      )
    )
  )

  private lazy val booleanLiteralsExample = TestCase(
    "boolean literals are recognized correctly",
    code =
      """[] > main
        |  TRUE > a_true
        |  FALSE > a_false
        |""".stripMargin,
    ast = Some(
      EOProg(
        metas = EOMetas(None, Vector()),
        bnds = Vector(
          EOBndExpr(
            bndName = EOAnyNameBnd(LazyName("main")),
            expr = Fix(
              EOObj(
                Vector(),
                None,
                Vector(
                  EOBndExpr(
                    EOAnyNameBnd(LazyName("a_true")),
                    Fix[EOExpr](EOBoolData(true))
                  ),
                  EOBndExpr(
                    EOAnyNameBnd(LazyName("a_false")),
                    Fix[EOExpr](EOBoolData(false)),
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  private lazy val mutualRecursionExample: TestCase[EOProg[Fix[EOExpr]]] =
    TestCase(
      label = "mutual recursion example",
      code =
        """+package sandbox
          |+alias stdout org.eolang.io.stdout
          |+alias sprintf org.eolang.txt.sprintf
          |[] > base
          |  memory > x
          |  [self v] > f
          |    x.write > @
          |      v
          |  [self v] > g
          |    self.f > @
          |      self
          |      v
          |[] > derived
          |  base > @
          |  [self v] > f
          |    self.g > @
          |      self
          |      v
          |""".stripMargin,
      ast = Some(
        EOProg(
          EOMetas(
            pack = Some("sandbox"),
            metas = Vector(
              EOAliasMeta(
                Some("stdout"),
                NonEmptyList("org", List("eolang", "io", "stdout"))
              ),
              EOAliasMeta(
                Some("sprintf"),
                NonEmptyList("org", List("eolang", "txt", "sprintf")),
              )
            )
          ),
          Vector(
            EOBndExpr(
              EOAnyNameBnd(LazyName("base")),
              Fix[EOExpr](
                EOObj(
                  freeAttrs = Vector(),
                  varargAttr = None,
                  bndAttrs = Vector(
                    EOBndExpr(
                      EOAnyNameBnd(LazyName("x")),
                      Fix[EOExpr](EOSimpleApp("memory"))
                    ),
                    EOBndExpr(
                      EOAnyNameBnd(LazyName("f")),
                      Fix[EOExpr](
                        EOObj(
                          freeAttrs = Vector(LazyName("self"), LazyName("v")),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              EODecoration,
                              Fix[EOExpr](
                                EOCopy(
                                  Fix[EOExpr](
                                    EODot(
                                      Fix[EOExpr](EOSimpleApp("x")),
                                      "write"
                                    )
                                  ),
                                  NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                    EOAnonExpr(Fix[EOExpr](EOSimpleApp("v")))
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    ),
                    EOBndExpr(
                      EOAnyNameBnd(LazyName("g")),
                      Fix[EOExpr](
                        EOObj(
                          freeAttrs = Vector(LazyName("self"), LazyName("v")),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              EODecoration,
                              Fix[EOExpr](
                                EOCopy(
                                  Fix[EOExpr](
                                    EODot(Fix[EOExpr](EOSimpleApp("self")), "f")
                                  ),
                                  NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                    EOAnonExpr(
                                      Fix[EOExpr](EOSimpleApp("self"))
                                    ),
                                    EOAnonExpr(Fix[EOExpr](EOSimpleApp("v")))
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
            EOBndExpr(
              EOAnyNameBnd(LazyName("derived")),
              Fix[EOExpr](
                EOObj(
                  freeAttrs = Vector(),
                  varargAttr = None,
                  bndAttrs = Vector(
                    EOBndExpr(EODecoration, Fix[EOExpr](EOSimpleApp("base"))),
                    EOBndExpr(
                      EOAnyNameBnd(LazyName("f")),
                      Fix[EOExpr](
                        EOObj(
                          freeAttrs = Vector(LazyName("self"), LazyName("v")),
                          varargAttr = None,
                          bndAttrs = Vector(
                            EOBndExpr(
                              EODecoration,
                              Fix[EOExpr](
                                EOCopy(
                                  Fix[EOExpr](
                                    EODot(Fix[EOExpr](EOSimpleApp("self")), "g")
                                  ),
                                  NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                    EOAnonExpr(
                                      Fix[EOExpr](EOSimpleApp("self"))
                                    ),
                                    EOAnonExpr(Fix[EOExpr](EOSimpleApp("v")))
                                  )
                                )
                              )
                            )
                          ),
                        )
                      )
                    )
                  )
                )
              )
            ),
          )
        )
      ),
    )

}
