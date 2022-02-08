package org.polystat.odin.analysis.inlining

object SetLocatorsTestCases {

  case class LocatorTestCase(
    label: String,
    codeBefore: String,
    codeAfter: String,
  )

  val vitaliyTest: LocatorTestCase = LocatorTestCase(
    label = "Test by Vitaliy",
    codeBefore =
      """[] > a
        |  [self y] > x
        |    y > @
        |  [self x y] > f
        |    self.g > h
        |      self
        |      x
        |    [] > @
        |      self.g > z
        |        self
        |        y
        |  [self z] > g
        |    x > k
        |    z > l
        |    [] > @
        |      l > a
        |      k > b
        |      z > c
        |      self > d
        |""".stripMargin,
    codeAfter =
      """[] > a
        |  [self y] > x
        |    $.y > @
        |  [self x y] > f
        |    $.self.g > h
        |      $.self
        |      $.x
        |    [] > @
        |      ^.self.g > z
        |        ^.self
        |        ^.y
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

  val nikolayTest: LocatorTestCase = LocatorTestCase(
    label = "Test by Nikolay",
    codeBefore =
      """[] > outer
        |  [] > self
        |    256 > magic
        |    [] > dummy
        |      [self] > aboba
        |        22 > @
        |      [self outer] > innerMethod
        |        self.aboba > @
        |          self
        |      self.innerMethod > @
        |        self
        |        self
        |    self > @
        |      "yahoo"
        |  [self] > method
        |    self.magic > @
        |""".stripMargin,
    codeAfter =
      """[] > outer
        |  [] > self
        |    256 > magic
        |    [] > dummy
        |      [self] > aboba
        |        22 > @
        |      [self outer] > innerMethod
        |        $.self.aboba > @
        |          $.self
        |      ^.^.self.innerMethod > @
        |        ^.^.self
        |        ^.^.self
        |    ^.self > @
        |      "yahoo"
        |  [self] > method
        |    $.self.magic > @
        |""".stripMargin
  )

}
