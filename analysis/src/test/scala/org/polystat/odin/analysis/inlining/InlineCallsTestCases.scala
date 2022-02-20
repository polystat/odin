package org.polystat.odin.analysis.inlining

import cats.data.EitherNel
import cats.syntax.either._
import cats.data.{NonEmptyList => Nel}

object InlineCallsTestCases {

  case class InliningTestCase(
    label: String,
    codeBefore: String,
    codeAfter: EitherNel[String, String]
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
        |    $.l > dummy
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
        |    [] > local_g
        |      ^.^.x > k
        |      ^.x > l
        |      $.l > dummy
        |    [] > h
        |      ^.local_g.l > a
        |      ^.local_g.k > b
        |      ^.x > c
        |      ^.self > d
        |    [] > @
        |      [] > local_g
        |        ^.^.^.x > k
        |        ^.^.y > l
        |        $.l > dummy
        |      [] > z
        |        ^.local_g.l > a
        |        ^.local_g.k > b
        |        ^.^.y > c
        |        ^.^.self > d
        |  [self z] > g
        |    ^.x > k
        |    $.z > l
        |    $.l > dummy
        |    [] > @
        |      ^.l > a
        |      ^.k > b
        |      ^.z > c
        |      ^.self > d
        |""".stripMargin.asRight
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
        |  ^.self > @
        |    "yahoo"
        |  [self] > method
        |    $.self.magic > @
        |""".stripMargin.asRight
  )

  val fakeCallTest: InliningTestCase = InliningTestCase(
    label = "Fake call, should not be transformed",
    codeBefore =
      """[] > object
        |  123 > self
        |  [self] > fake_call
        |    ^.self.add ^.self > @
        |
        |  # fake add
        |  [self] > add
        |    $.self > @
        |""".stripMargin,
    codeAfter =
      """[] > object
        |  123 > self
        |  [self] > fake_call
        |    ^.self.add > @
        |      ^.self
        |  [self] > add
        |    $.self > @
        |""".stripMargin.asRight
  )

  val looksFakeButRealTest: InliningTestCase = InliningTestCase(
    label = "Similar to fake call, but is real and should be transformed",
    codeBefore =
      """[] > object
        |  123 > self
        |  [self] > fake_call
        |    $.self.add $.self > @
        |  # fake add
        |  [self] > add
        |    $.self > @
        |""".stripMargin,
    codeAfter =
      """[] > object
        |  123 > self
        |  [self] > fake_call
        |    $.self > @
        |  [self] > add
        |    $.self > @
        |""".stripMargin.asRight
  )

  val factorialTest: InliningTestCase = InliningTestCase(
    label = "An object with a single method that calls itself",
    codeBefore =
      """[] > factorial
        |  [self i] > calculate
        |    ($.i.less 2).if > @
        |      1
        |      $.i.mul
        |        $.self.calculate
        |          $.self
        |          $.i.sub
        |            1
        |""".stripMargin,
    codeAfter =
      """[] > factorial
        |  [self i] > calculate
        |    ($.i.less 2).if > @
        |      1
        |      $.i.mul
        |        (($.i.sub 1).less 2).if
        |          1
        |          ($.i.sub 1).mul
        |            $.self.calculate
        |              $.self
        |              ($.i.sub 1).sub
        |                1
        |""".stripMargin.asRight
  )

  val evenOddTest: InliningTestCase = InliningTestCase(
    label = "An object with 2 mutually recursive methods",
    codeBefore =
      """
        |# 1 - true
        |# 0 - false
        |# Booleans are not parsed yet, sorry((
        |# Issue link: https://github.com/polystat/odin/issues/31
        |[] > numeric_ops
        |  [self n] > is_even
        |    ($.n.eq 0).if > @
        |      1
        |      $.self.is_odd
        |        $.self
        |        ($.n.sub 1)
        |  [self n] > is_odd
        |    ($.n.eq 0).if > @
        |      0
        |      $.self.is_even
        |        $.self
        |        ($.n.sub 1)
        |""".stripMargin,
    codeAfter =
      """[] > numeric_ops
        |  [self n] > is_even
        |    ($.n.eq 0).if > @
        |      1
        |      (($.n.sub 1).eq 0).if
        |        0
        |        $.self.is_even
        |          $.self
        |          ($.n.sub 1).sub
        |            1
        |  [self n] > is_odd
        |    ($.n.eq 0).if > @
        |      0
        |      (($.n.sub 1).eq 0).if
        |        1
        |        $.self.is_odd
        |          $.self
        |          ($.n.sub 1).sub
        |            1
        |""".stripMargin.asRight
  )

  val average3Test: InliningTestCase = InliningTestCase(
    label = "average of 3 numbers",
    codeBefore =
      """
        |[] > obj
        |  [self arg1 arg2 arg3] > average3
        |    arg1.add (arg2.add arg3) > sum
        |    3 > count
        |    sum.div count > @
        |  [self] > call-site
        |    self.average3 self 1 2 3 > @
        |""".stripMargin,
    codeAfter =
      """[] > obj
        |  [self arg1 arg2 arg3] > average3
        |    $.arg1.add > sum
        |      $.arg2.add
        |        $.arg3
        |    3 > count
        |    $.sum.div > @
        |      $.count
        |  [self] > call-site
        |    [] > local_average3
        |      1.add > sum
        |        2.add
        |          3
        |      3 > count
        |    $.local_average3.sum.div > @
        |      $.local_average3.count
        |""".stripMargin.asRight
  )

  val average3WithComponentsTest: InliningTestCase = InliningTestCase(
    label = "average3 that also returns the sum and count",
    codeBefore =
      """[] > obj
        |  [self arg1 arg2 arg3] > average3
        |    arg1.add (arg2.add arg3) > sum
        |    3 > count
        |    sum.div count > average
        |    [] > @
        |      sum > sum
        |      count > count
        |      average > average
        |  [self] > call-site
        |    self.average3 self 1 2 3 > @
        |""".stripMargin,
    codeAfter =
      """[] > obj
        |  [self arg1 arg2 arg3] > average3
        |    $.arg1.add > sum
        |      $.arg2.add
        |        $.arg3
        |    3 > count
        |    $.sum.div > average
        |      $.count
        |    [] > @
        |      ^.sum > sum
        |      ^.count > count
        |      ^.average > average
        |  [self] > call-site
        |    [] > local_average3
        |      1.add > sum
        |        2.add
        |          3
        |      3 > count
        |      $.sum.div > average
        |        $.count
        |    [] > @
        |      ^.local_average3.sum > sum
        |      ^.local_average3.count > count
        |      ^.local_average3.average > average
        |""".stripMargin.asRight
  )

  val simpleTests: List[InliningTestCase] = List(
    InliningTestCase(
      label = "simple test 1",
      codeBefore =
        """
          |[] > obj
          |  [self arg1 arg2 arg3] > method
          |    arg1.add arg2 > @
          |  [self] > call-site
          |    self.method self 1 2 3 > @
          |""".stripMargin,
      codeAfter =
        """[] > obj
          |  [self arg1 arg2 arg3] > method
          |    $.arg1.add > @
          |      $.arg2
          |  [self] > call-site
          |    1.add > @
          |      2
          |""".stripMargin.asRight
    ),
  )

  val notEnoughArgs: InliningTestCase = InliningTestCase(
    label = "not enough arguments passed to a method call",
    codeBefore =
      """[] > obj
        |  [self arg1 arg2 arg3] > method
        |    arg1.add arg2 > @
        |  [self] > call-site
        |    self.method self 1 2 > @
        |""".stripMargin,
    codeAfter = Nel.one("Wrong number of arguments given for method method.").asLeft
  )

  val tooManyArgs: InliningTestCase = InliningTestCase(
    label = "too many argument passed to a method call",
    codeBefore =
      """
        |[] > obj
        |  [self arg1 arg2 arg3] > method
        |    arg1.add arg2 > @
        |  [self] > call-site
        |    self.method self 1 2 3 4 5 > @
        |""".stripMargin,
    codeAfter = Nel.one("Wrong number of arguments given for method method.").asLeft
  )

}
