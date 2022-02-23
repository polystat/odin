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

}
