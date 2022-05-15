package org.polystat.odin.analysis.inlining

import cats.data.{EitherNel, NonEmptyList => Nel}
import cats.syntax.either._

object SetLocatorsTestCases {

  case class LocatorTestCase(
    label: String,
    codeBefore: String,
    codeAfter: EitherNel[String, String],
  )

  val vitaliyTestLocators: LocatorTestCase = LocatorTestCase(
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
        |""".stripMargin.asRight
  )

  val nikolayTestLocators: LocatorTestCase = LocatorTestCase(
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
        |""".stripMargin.asRight
  )

  val nonExistentNameTestLocators: LocatorTestCase = LocatorTestCase(
    label = "self is not defined anywhere; should fail",
    codeBefore =
      """[] > outer
        |  256 > magic
        |  self "yahoo" > @
        |  [self] > method
        |    self.magic > @
        |""".stripMargin,
    codeAfter = Nel
      .one("Could not set locator for non-existent object with name \"self\"")
      .asLeft
  )

  val builtinObjects: LocatorTestCase = LocatorTestCase(
    label = "locators are set correctly for seq and assert",
    codeBefore =
      """
        |[] > a
        |  assert ("true") > @
        |  [] > b
        |    assert ("false") > @
        |    [] > c
        |      seq > @
        |        seq
        |          assert (0.less 1)
        |          assert (0.less 2)
        |""".stripMargin,
    codeAfter =
      """[] > a
        |  ^.assert > @
        |    "true"
        |  [] > b
        |    ^.^.assert > @
        |      "false"
        |    [] > c
        |      ^.^.^.seq > @
        |        ^.^.^.seq
        |          ^.^.^.assert
        |            0.less
        |              1
        |          ^.^.^.assert
        |            0.less
        |              2
        |""".stripMargin.asRight
  )

  val autoGenerated: LocatorTestCase = LocatorTestCase(
    label = "generated by mutual recursion test generator",
    codeBefore = """[] > u
                   |  [self] > u
                   |    self > @
                   |  [self] > i
                   |    self > @
                   |  [self] > t
                   |    self > @
                   |  [self] > z
                   |    self.u self > @
                   |  
                   |[] > km
                   |  ^.u > @
                   |  [self] > j
                   |    self.z self > @
                   |  [self] > i
                   |    self.z self > @
                   |  [self] > u
                   |    self.i self > @
                   |  [self] > t
                   |    self > @
                   |  [] > jr
                   |
                   |    
                   |  [] > ie
                   |    [self] > z
                   |      self > @
                   |    [self] > d
                   |      self.z self > @
                   |    
                   |  [] > w
                   |    [self] > x
                   |      self > @
                   |    [self] > l
                   |      self > @
                   |""".stripMargin,
    codeAfter =
      """[] > u
        |  [self] > u
        |    $.self > @
        |  [self] > i
        |    $.self > @
        |  [self] > t
        |    $.self > @
        |  [self] > z
        |    $.self.u > @
        |      $.self
        |[] > km
        |  ^.u > @
        |  [self] > j
        |    $.self.z > @
        |      $.self
        |  [self] > i
        |    $.self.z > @
        |      $.self
        |  [self] > u
        |    $.self.i > @
        |      $.self
        |  [self] > t
        |    $.self > @
        |  [] > jr
        |  [] > ie
        |    [self] > z
        |      $.self > @
        |    [self] > d
        |      $.self.z > @
        |        $.self
        |  [] > w
        |    [self] > x
        |      $.self > @
        |    [self] > l
        |      $.self > @
        |""".stripMargin.asRight
  )

  val withAliases: LocatorTestCase = LocatorTestCase(
    label = "setting locators for aliases (imports)",
    codeBefore =
      """
        |+alias org.eolang.io.stdout
        |+alias print org.eolang.txt.sprintf
        |[] > a
        |  [self] > method
        |    stdout > out
        |    seq > @
        |      print "output"
        |      22
        |""".stripMargin,
    codeAfter =
      """+alias org.eolang.io.stdout
        |+alias print org.eolang.txt.sprintf
        |
        |[] > a
        |  [self] > method
        |    ^.^.stdout > out
        |    ^.^.seq > @
        |      ^.^.print
        |        "output"
        |      22
        |""".stripMargin.asRight
  )

  val withPredef: LocatorTestCase = LocatorTestCase(
    label = "setting locators for EO keywords (seq, assert, random)",
    codeBefore =
      """
        |[] > a
        |  [self] > method
        |    random > num
        |    memory > var
        |    cage > what
        |    seq > @
        |      goto "somewhere"
        |      assert (3.less 10)
        |      22
        |""".stripMargin,
    codeAfter =
      """[] > a
        |  [self] > method
        |    ^.^.random > num
        |    ^.^.memory > var
        |    ^.^.cage > what
        |    ^.^.seq > @
        |      ^.^.goto
        |        "somewhere"
        |      ^.^.assert
        |        3.less
        |          10
        |      22
        |""".stripMargin.asRight
  )

}
