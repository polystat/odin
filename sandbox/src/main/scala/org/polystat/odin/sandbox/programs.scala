package org.polystat.odin.sandbox

import cats.data.NonEmptyList
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

object programs {
  val mutualRecursionExample: EOProg[EOExprOnly] = EOProg(
    EOMetas(
      pack = Some("sandbox"),
      metas = Vector(
        EOAliasMeta(Some("stdout"), NonEmptyList("org", List("eolang", "io", "stdout"))),
        EOAliasMeta(Some("sprintf"), NonEmptyList("org", List("eolang", "txt", "sprintf"))),
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
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("x")), "write")),
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
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("self")), "f")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("self"))),
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
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("self")), "g")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("self"))),
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
}
