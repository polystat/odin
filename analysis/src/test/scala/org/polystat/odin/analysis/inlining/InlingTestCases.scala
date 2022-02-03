package org.polystat.odin.analysis.inlining

import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

object InlingTestCases {

  case class LocatorTestCase(
    label: String,
    codeBefore: EOProg[EOExprOnly],
    codeAfter: EOProg[EOExprOnly],
  )

  val vitaliyTest: LocatorTestCase = LocatorTestCase(
    label = "Test by Vitaliy",
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

  val nikolayTest: LocatorTestCase = LocatorTestCase(
    label = "Test by Nikolay",
    codeBefore = EOProg(
      metas = EOMetas(pack = None, metas = Vector()),
      bnds = Vector(
        EOBndExpr(
          bndName = EOAnyNameBnd(name = LazyName(name = "a")),
          expr = Fix(
            EOObj(
              freeAttrs = Vector(),
              varargAttr = None,
              bndAttrs = Vector(
                EOBndExpr(
                  bndName = EOAnyNameBnd(name = LazyName(name = "x")),
                  expr = Fix(
                    EOObj(
                      freeAttrs =
                        Vector(LazyName(name = "self"), LazyName(name = "y")),
                      varargAttr = None,
                      bndAttrs = Vector(
                        EOBndExpr(
                          bndName = EODecoration,
                          expr = Fix[EOExpr](EOSimpleApp(name = "y"))
                        )
                      )
                    )
                  )
                ),
                EOBndExpr(
                  bndName = EOAnyNameBnd(name = LazyName(name = "f")),
                  expr = Fix(
                    EOObj(
                      freeAttrs = Vector(
                        LazyName(name = "self"),
                        LazyName(name = "x"),
                        LazyName(name = "y")
                      ),
                      varargAttr = None,
                      bndAttrs = Vector(
                        EOBndExpr(
                          bndName = EOAnyNameBnd(name = LazyName(name = "h")),
                          expr = Fix(
                            EOCopy(
                              trg =
                                Fix(
                                  EODot(
                                    src =
                                      Fix[EOExpr](EOSimpleApp(name = "self")),
                                    name = "g"
                                  )
                                ),
                              args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                EOAnonExpr(Fix[EOExpr](EOSimpleApp("self"))),
                                EOAnonExpr(Fix[EOExpr](EOSimpleApp("x")))
                              )
                            )
                          )
                        ),
                        EOBndExpr(
                          bndName = EODecoration,
                          expr = Fix(
                            EOObj(
                              freeAttrs = Vector(),
                              varargAttr = None,
                              bndAttrs = Vector(
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "z")),
                                  expr = Fix(
                                    EOCopy(
                                      trg = Fix(
                                        EODot(
                                          src = Fix[EOExpr](
                                            EOSimpleApp(name = "self")
                                          ),
                                          name = "g"
                                        )
                                      ),
                                      args =
                                        NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                          EOAnonExpr(
                                            Fix[EOExpr](EOSimpleApp("self"))
                                          ),
                                          EOAnonExpr(
                                            Fix[EOExpr](EOSimpleApp("y"))
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
                EOBndExpr(
                  bndName = EOAnyNameBnd(name = LazyName(name = "g")),
                  expr = Fix(
                    EOObj(
                      freeAttrs =
                        Vector(LazyName(name = "self"), LazyName(name = "z")),
                      varargAttr = None,
                      bndAttrs = Vector(
                        EOBndExpr(
                          bndName = EOAnyNameBnd(name = LazyName(name = "k")),
                          expr = Fix[EOExpr](EOSimpleApp(name = "x"))
                        ),
                        EOBndExpr(
                          bndName = EOAnyNameBnd(name = LazyName(name = "l")),
                          expr = Fix[EOExpr](EOSimpleApp(name = "z"))
                        ),
                        EOBndExpr(
                          bndName = EODecoration,
                          expr = Fix(
                            EOObj(
                              freeAttrs = Vector(),
                              varargAttr = None,
                              bndAttrs = Vector(
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "a")),
                                  expr = Fix[EOExpr](EOSimpleApp(name = "l"))
                                ),
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "b")),
                                  expr = Fix[EOExpr](EOSimpleApp(name = "k"))
                                ),
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "c")),
                                  expr = Fix[EOExpr](EOSimpleApp(name = "z"))
                                ),
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "d")),
                                  expr = Fix[EOExpr](EOSimpleApp(name = "self"))
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
    ),
    codeAfter = EOProg(
      metas = EOMetas(pack = None, metas = Vector()),
      bnds = Vector(
        EOBndExpr(
          bndName = EOAnyNameBnd(name = LazyName(name = "a")),
          expr = Fix(
            EOObj(
              freeAttrs = Vector(),
              varargAttr = None,
              bndAttrs = Vector(
                EOBndExpr(
                  bndName = EOAnyNameBnd(name = LazyName(name = "x")),
                  expr = Fix(
                    EOObj(
                      freeAttrs =
                        Vector(LazyName(name = "self"), LazyName(name = "y")),
                      varargAttr = None,
                      bndAttrs = Vector(
                        EOBndExpr(
                          bndName = EODecoration,
                          expr = Fix[EOExpr](
                            EOSimpleAppWithLocator(name = "y", locator = 0)
                          )
                        )
                      )
                    )
                  )
                ),
                EOBndExpr(
                  bndName = EOAnyNameBnd(name = LazyName(name = "f")),
                  expr = Fix(
                    EOObj(
                      freeAttrs = Vector(
                        LazyName(name = "self"),
                        LazyName(name = "x"),
                        LazyName(name = "y")
                      ),
                      varargAttr = None,
                      bndAttrs = Vector(
                        EOBndExpr(
                          bndName = EOAnyNameBnd(name = LazyName(name = "h")),
                          expr = Fix(
                            EOCopy(
                              trg =
                                Fix(
                                  EODot(
                                    src =
                                      Fix[EOExpr](
                                        EOSimpleAppWithLocator(
                                          name = "self",
                                          locator = 0
                                        )
                                      ),
                                    name = "g"
                                  )
                                ),
                              args = NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                EOAnonExpr(
                                  Fix[EOExpr](
                                    EOSimpleAppWithLocator(
                                      name = "self",
                                      locator = 0
                                    )
                                  )
                                ),
                                EOAnonExpr(
                                  Fix[EOExpr](
                                    EOSimpleAppWithLocator(
                                      name = "x",
                                      locator = 0
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
                            EOObj(
                              freeAttrs = Vector(),
                              varargAttr = None,
                              bndAttrs = Vector(
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "z")),
                                  expr = Fix(
                                    EOCopy(
                                      trg = Fix(
                                        EODot(
                                          src = Fix[EOExpr](
                                            EOSimpleAppWithLocator(
                                              name = "self",
                                              locator = 1
                                            )
                                          ),
                                          name = "g"
                                        )
                                      ),
                                      args =
                                        NonEmpty[Vector[EOBnd[EOExprOnly]]](
                                          EOAnonExpr(
                                            Fix[EOExpr](
                                              EOSimpleAppWithLocator(
                                                name = "self",
                                                locator = 1
                                              )
                                            )
                                          ),
                                          EOAnonExpr(
                                            Fix[EOExpr](
                                              EOSimpleAppWithLocator(
                                                name = "y",
                                                locator = 1
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
                ),
                EOBndExpr(
                  bndName = EOAnyNameBnd(name = LazyName(name = "g")),
                  expr = Fix(
                    EOObj(
                      freeAttrs =
                        Vector(LazyName(name = "self"), LazyName(name = "z")),
                      varargAttr = None,
                      bndAttrs = Vector(
                        EOBndExpr(
                          bndName = EOAnyNameBnd(name = LazyName(name = "k")),
                          expr = Fix[EOExpr](
                            EOSimpleAppWithLocator(name = "x", locator = 1)
                          )
                        ),
                        EOBndExpr(
                          bndName = EOAnyNameBnd(name = LazyName(name = "l")),
                          expr = Fix[EOExpr](
                            EOSimpleAppWithLocator(name = "z", locator = 0)
                          )
                        ),
                        EOBndExpr(
                          bndName = EODecoration,
                          expr = Fix(
                            EOObj(
                              freeAttrs = Vector(),
                              varargAttr = None,
                              bndAttrs = Vector(
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "a")),
                                  expr = Fix[EOExpr](
                                    EOSimpleAppWithLocator(
                                      name = "l",
                                      locator = 1
                                    )
                                  )
                                ),
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "b")),
                                  expr = Fix[EOExpr](
                                    EOSimpleAppWithLocator(
                                      name = "k",
                                      locator = 1
                                    )
                                  )
                                ),
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "c")),
                                  expr = Fix[EOExpr](
                                    EOSimpleAppWithLocator(
                                      name = "z",
                                      locator = 1
                                    )
                                  )
                                ),
                                EOBndExpr(
                                  bndName =
                                    EOAnyNameBnd(name = LazyName(name = "d")),
                                  expr = Fix[EOExpr](
                                    EOSimpleAppWithLocator(
                                      name = "self",
                                      locator = 1
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

}
