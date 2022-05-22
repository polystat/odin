package org.polystat.odin.parser.ast_tests

import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.TestUtils.TestCase

object SingleLineExamples {

  val correct: List[TestCase[EOExprOnly]] =
    List(
      justA,
      normalApp,
      rightAssoc,
      leftAssoc,
      singleLineAbstraction,
    )

  private lazy val singleLineAbstraction: TestCase[EOExprOnly] =
    TestCase(
      label = "single line abstraction",
      code = "[x] (x.add 1 > succ) (x.sub 1 > prev)",
      ast = Some(
        Fix(
          EOObj(
            freeAttrs = Vector(LazyName("x")),
            varargAttr = None,
            bndAttrs = Vector(
              EOBndExpr(
                bndName = EOAnyNameBnd(LazyName("succ")),
                expr = Fix(
                  EOCopy(
                    trg = Fix(
                      EODot(
                        src = Fix(EOSimpleApp[EOExprOnly]("x")),
                        name = "add"
                      )
                    ),
                    args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                      EOAnonExpr(Fix(EOIntData[EOExprOnly](1)))
                    )
                  )
                )
              ),
              EOBndExpr(
                bndName = EOAnyNameBnd(LazyName("prev")),
                expr = Fix(
                  EOCopy(
                    trg = Fix(
                      EODot(
                        src = Fix(EOSimpleApp[EOExprOnly]("x")),
                        name = "sub"
                      )
                    ),
                    args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                      EOAnonExpr(Fix(EOIntData[EOExprOnly](1)))
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

  private lazy val justA: TestCase[Fix[EOExpr]] = TestCase(
    label = "justA",
    code = "a",
    ast = Some(Fix[EOExpr](EOSimpleApp("a")))
  )

  private lazy val normalApp: TestCase[Fix[EOExpr]] = TestCase(
    label = "normal application",
    code = "a b c d",
    ast = Some(
      Fix[EOExpr](
        EOCopy(
          Fix[EOExpr](EOSimpleApp("a")),
          NonEmpty[Vector[EOBnd[EOExprOnly]]](
            EOAnonExpr(Fix[EOExpr](EOSimpleApp("b"))),
            EOAnonExpr(Fix[EOExpr](EOSimpleApp("c"))),
            EOAnonExpr(Fix[EOExpr](EOSimpleApp("d")))
          )
        )
      )
    )
  )

  private lazy val rightAssoc: TestCase[Fix[EOExpr]] = TestCase(
    label = "rightAssociative",
    code = "a (b (c d))",
    ast = Some(
      Fix[EOExpr](
        EOCopy(
          Fix[EOExpr](EOSimpleApp("a")),
          NonEmpty[Vector[EOBnd[EOExprOnly]]](
            EOAnonExpr(
              Fix[EOExpr](
                EOCopy(
                  Fix[EOExpr](EOSimpleApp("b")),
                  NonEmpty[Vector[EOBnd[EOExprOnly]]](
                    EOAnonExpr(
                      Fix[EOExpr](
                        EOCopy(
                          Fix[EOExpr](EOSimpleApp("c")),
                          NonEmpty[Vector[EOBnd[EOExprOnly]]](
                            EOAnonExpr(Fix[EOExpr](EOSimpleApp("d")))
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

  private lazy val leftAssoc: TestCase[Fix[EOExpr]] = TestCase(
    label = "leftAssociative",
    code = "((a b) c) d",
    ast = Some(
      Fix[EOExpr](
        EOCopy(
          Fix[EOExpr](
            EOCopy(
              Fix[EOExpr](
                EOCopy(
                  Fix[EOExpr](EOSimpleApp("a")),
                  NonEmpty[Vector[EOBnd[EOExprOnly]]](
                    EOAnonExpr(Fix[EOExpr](EOSimpleApp("b")))
                  )
                )
              ),
              NonEmpty[Vector[EOBnd[EOExprOnly]]](
                EOAnonExpr(Fix[EOExpr](EOSimpleApp("c")))
              )
            )
          ),
          NonEmpty[Vector[EOBnd[EOExprOnly]]](
            EOAnonExpr(Fix[EOExpr](EOSimpleApp("d")))
          )
        )
      )
    )
  )

}
