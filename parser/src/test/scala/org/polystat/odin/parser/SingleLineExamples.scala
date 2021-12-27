package org.polystat.odin.parser

import com.github.tarao.nonempty.collection.NonEmpty
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import higherkindness.droste.data.Fix
import org.polystat.odin.parser.TestUtils.TestCase

object SingleLineExamples {

  val correct: List[TestCase[EOExprOnly]] =
    List(
      TestCase(
        label = "justA",
        code = "a",
        ast = Some(Fix[EOExpr](EOSimpleApp("a")))
      ),
      TestCase(
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
      ),
      TestCase(
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
      ),
      TestCase(
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
    )

}
