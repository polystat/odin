package eo.sandbox

import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast._
import eo.core.ast.astparams.EOExprOnly
import eo.utils.func.Fix

object programs {
  val mutualRecursionExample: EOProg[EOExprOnly] = EOProg(
    EOMetas(
      pack = Some("sandbox"),
      metas = Vector(
        EOAliasMeta("stdout", "org.eolang.io.stdout"),
        EOAliasMeta("sprintf", "org.eolang.txt.sprintf"),
      )
    ),
    Vector(
      EOBndExpr[EOExprOnly](
        EOAnyNameBnd(LazyBnd("base")),
        Fix[EOExpr](
          EOObj(
            freeAttrs = Vector(),
            varargAttr = None,
            bndAttrs = Vector(
              EOBndExpr[EOExprOnly](
                EOAnyNameBnd(LazyBnd("x")),
                Fix[EOExpr](EOSimpleApp("memory"))
              ),
              EOBndExpr[EOExprOnly](
                EOAnyNameBnd(LazyBnd("f")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyBnd("self"), LazyBnd("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EOBndExpr[EOExprOnly](
                        EODecoration(),
                        Fix[EOExpr](
                          EOCopy(
                            Fix[EOExpr](EODot[EOExprOnly](Fix[EOExpr](EOSimpleApp("x")), "write")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr[EOExprOnly](Fix[EOExpr](EOSimpleApp("v")))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              EOBndExpr[EOExprOnly](
                EOAnyNameBnd(LazyBnd("g")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyBnd("self"), LazyBnd("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EOBndExpr[EOExprOnly](
                        EODecoration(),
                        Fix[EOExpr](
                          EOCopy(
                            Fix[EOExpr](EODot[EOExprOnly](Fix[EOExpr](EOSimpleApp("self")), "f")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr[EOExprOnly](Fix[EOExpr](EOSimpleApp("self"))),
                              EOAnonExpr[EOExprOnly](Fix[EOExpr](EOSimpleApp("v")))
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
        EOAnyNameBnd(LazyBnd("derived")),
        Fix[EOExpr](
          EOObj(
            freeAttrs = Vector(),
            varargAttr = None,
            bndAttrs = Vector(
              EOBndExpr(EODecoration(), Fix[EOExpr](EOSimpleApp("base"))),
              EOBndExpr(
                EOAnyNameBnd(LazyBnd("f")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyBnd("self"), LazyBnd("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EOBndExpr[EOExprOnly](
                        EODecoration(),
                        Fix[EOExpr](
                          EOCopy[EOExprOnly](
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("self")), "g")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr[EOExprOnly](Fix[EOExpr](EOSimpleApp("self"))),
                              EOAnonExpr[EOExprOnly](Fix[EOExpr](EOSimpleApp("v")))
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
