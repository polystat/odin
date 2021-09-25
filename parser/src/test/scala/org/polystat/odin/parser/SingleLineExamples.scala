package org.polystat.odin.parser

import com.github.tarao.nonempty.collection.NonEmpty
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import higherkindness.droste.data.Fix

object SingleLineExamples {

  val correct: List[(String, (String, EOExprOnly))] =
    List(
      (
        "justA",
        (
          "a",
          Fix[EOExpr](EOSimpleApp("a"))
        )
      ),
      (
        "normal application",
        (
          "a b c d",
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
      (
        "rightAssociative",
        (
          "a (b (c d))",
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
      (
        "leftAssociative",
        (
          "((a b) c) d",
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
