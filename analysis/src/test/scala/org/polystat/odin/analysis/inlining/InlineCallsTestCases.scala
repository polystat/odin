package org.polystat.odin.analysis.inlining

object InlineCallsTestCases {

  case class InliningTestCase(
    label: String,
    codeBefore: String,
    codeAfter: String
  )

  val nikolayTestInlining: InliningTestCase = InliningTestCase(
    label = "Inlining testcase by Nikolay",
    codeBefore =
      """[] > a
        |  [self y] > x
        |    $.y > @
        |  [self x y] > f
        |    $.self.g $.self $.x > h
        |    [] > @
        |      ^.self.g ^.self ^.y > z
        |  [self z] > g
        |    ^.x > k
        |    $.z > l
        |    [] > @
        |      ^.l > a
        |      ^.k > b
        |      ^.z > c
        |      ^.self > d
        |""".stripMargin,
    codeAfter =
      """[] > a
        |  [self y] > x
        |    $.y > @
        |  [self x y] > f
        |    [] > local_g_0
        |      ^.^.x > k
        |      ^.x > l
        |    [] > h
        |      ^.local_g_0.l > a
        |      ^.local_g_0.k > b
        |      ^.x > c
        |      ^.self > d
        |    [] > @
        |      [] > local_g_0
        |        ^.^.^.x > k
        |        ^.^.y > l
        |      [] > z
        |        ^.local_g_0.l > a
        |        ^.local_g_0.k > b
        |        ^.^.y > c
        |        ^.^.self > d
        |  [self z] > g
        |    ^.x > k
        |    $.z > l
        |    [] > @
        |      ^.l > a
        |      ^.k > b
        |      ^.z > c
        |      ^.self > d
        |""".stripMargin
  )

  val vitaliyTestInlining: InliningTestCase = InliningTestCase(
    label = "Inlining testcase by Vitaliy",
    codeBefore =
      """[] > outer
        |  256 > magic
        |  [] > dummy
        |    [self] > bMethod
        |      22 > @
        |    [self outer] > innerMethod
        |      [self] > innerInnerMethod
        |        ^.self.bMethod ^.self > @
        |      $.self.bMethod $.self > @
        |    $.innerMethod 1 1 > b
        |  self "yahoo" > @
        |  [self] > method
        |    self.magic > @
        |""".stripMargin,
    codeAfter =
      """[] > outer
        |  256 > magic
        |  [] > dummy
        |    [self] > bMethod
        |      22 > @
        |    [self outer] > innerMethod
        |      [self] > innerInnerMethod
        |        22 > @
        |      22 > @
        |    $.innerMethod > b
        |      1
        |      1
        |  self > @
        |    "yahoo"
        |  [self] > method
        |    self.magic > @
        |""".stripMargin
  )

}
