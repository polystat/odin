package org.polystat.odin.parser.fastparse

import fastparse._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EOParserTestSuite
import org.polystat.odin.parser.TestUtils.{TestCase, astPrinter}
import org.polystat.odin.parser.fastparse.IgnoreEmptyLinesOrComments._
import org.scalatest.Assertion


class ParserTests extends EOParserTestSuite {

  def parseEntireInput[_: P, T](p: => P[T]): P[T] = P(Start ~ p ~ End)

  override type ParserT[A] = P[_] => P[A]
  override type Success[A] = A
  override type Error = Parsed.Failure


  override def checkParser[A](check: ParserResultT[A] => Boolean)
                             (parser: ParserT[A], input: String): Assertion = {
    val parsed = parse(input, parser) match {
      case Parsed.Success(value, _) => Right(value)
      case failure: Parsed.Failure => Left(failure)
    }
    parsed match {
      case Right(value) => astPrinter.pprintln(value)
      case Left(failure) => println(failure.trace())
    }
    assert(check(parsed))
  }

  override def programParser: ParserT[EOProg[EOExprOnly]] = new Parser().program(_)

  override def singleLineApplicationParser: ParserT[EOExprOnly] = SingleLineApplication.singleLineApplication(_)


  "tokens" should {
    "be recognized correctly" in {
      shouldParse(Tokens.emptyLine(_), "      \t\n")
      shouldParse(Tokens.comment(_), "#   oo 121 _= `12 e3\n")
      shouldParse(Tokens.identifier(_), "a-COOL-identifier")
      shouldParse(Tokens.string(_), "\" asd aa dd -= -21 123\"")
      shouldParse(Tokens.integer(_), "12345013")
      shouldParse(Tokens.integer(_), "-12345013")
      shouldParse(Tokens.char(_), "\'a\'")
    }
  }

  "metas" should {
    def metasAllInput[_: P]: P[EOMetas] = parseEntireInput(new Parser().metas)

    "be recognized correctly" in {
      shouldParse(metasAllInput(_),
        """
          |
          |# start
          |
          |+package sandbox
          |
          |# meta specifying the current runtime for eo
          |+rt jvm org.eolang:eo-runtime:0.1.24
          |
          |# meta used to rename imported packages
          |  # can be used to resolve name conflicts or
          |  # to make the names more succinct
          |+alias stdout org.eolang.io.stdout
          |
          |
          |# some comment here
          |# some comment
          |
          |
          |
          |+alias sprintf org.eolang.txt.sprintf
          |
          |+alias biba boba
          |""".stripMargin)
    }
  }

  "args" should {

    def argsAllInput[_: P]: P[(Vector[LazyName], Option[LazyName])] =
      parseEntireInput(SingleLineApplication.args)

    val correctArgsExamples:
      List[TestCase[(Vector[LazyName], Option[LazyName])]] =
      List(
        TestCase("[a b  c    d...]", "[a b  c    d...]"),
        TestCase("[a b c d]", "[a b c d]"),
        TestCase("[a b @]", "[a b @]"),
        TestCase("[@...]", "[@...]"),
        TestCase("[]", "[]")
      )

    val incorrectArgsExamples:
      List[TestCase[(Vector[LazyName], Option[LazyName])]] =
      List(
        TestCase("[", "[", None),
        TestCase("[...]", "[...]", None)
      )

    checkExamples(
      argsAllInput(_),
      correctExamples = correctArgsExamples,
      incorrectExamples = incorrectArgsExamples
    )


  }

  "abstraction" should {
    def anonymousAbstractionAllInput[_: P] =
      parseEntireInput(new AnonymousObjects().anonymousAbstraction)

    val correctExamples: Examples[EOAnonExpr[EOExprOnly]] = List(
      TestCase(
        label = "simplest possible object",
        code = "[]",
      ),

      TestCase(
        label = "many nested objects, formatted correctly",
        code =
          """[]
            |  [a b c] > a
            |  [@...] > freeDecoratee
            |    [] > stuff
            |      [] > mmm
            |      [] > asd
            |    [] > noArgs
            |    [someArgs] > someArg""".stripMargin
      ),

      TestCase(
        label = "shows the flexibility of whitespace",
        code =
          """[]
            |  # This is a
            |  [] > a
            |
            |  # This is b
            |
            |  [] > b
            |
            |
            |  # This is c.
            | # This line is here
            |    # to show that the comments can be
            |# multiline and don't have to be properly indented
            |  [] > c""".stripMargin
      )
    )

    val incorrectExamples: Examples[EOAnonExpr[EOExprOnly]] = List(
      TestCase(
        label = "simplest malformed object",
        code = "["
      ),

      TestCase(
        label = "object with inconsistent indentation",
        code =
          """[]
            |  [] > objects
            |  [] > however
            |   [] > haveTo
            |   [] > beProperlyIndented
            |
            |""".stripMargin
      )
    )

    checkExamples(
      anonymousAbstractionAllInput(_),
      correctExamples,
      incorrectExamples
    )

  }

  "application" should {
    def namedApplicationAllInput[_: P]: P[EOBndExpr[EOExprOnly]] =
      parseEntireInput(new NamedObjects().namedApplication)

    val correctExamples: List[TestCase[EOBndExpr[EOExprOnly]]] = List(
      TestCase(
        label = "some simple objects",
        code =
          """a > namedA
            |  b > namedB
            |    b2
            |  c > namedC
            |    c2
            |      c3 > namedC3
            |  []
            |    # some anonymous object
            |    [] > someArg
            |  d""".stripMargin,
        ast = None
      ),

      TestCase(
        label = "some data objects",
        code =
          """a > namedA
            |  123
            |  'a'
            |  "a huge long long long string"
            |  a""".stripMargin,
        ast = None
      ),

      TestCase(
        label = "some attribute chains",
        code =
          """a.b.c.d > namedA
            |  $.^.@.a > some-obj-from-outer-scope
            |  1.neg""".stripMargin,
      ),
      TestCase(
        label = "some single line applications",
        code =
          """(a) > named-a-with-redundant-parentheses
            |
            |  # a applied to b, c and
            |  a b c d > normalApplication
            |
            |  # a applied to (b applied to (c applied to d))
            |  a (b (c d)) > cascadingApplication
            |
            |  # this a reverse application:
            |  # ((a applied to b) applied to c)
            |  ((a b) c) d > reverseApplication""".stripMargin
      ),

      TestCase(
        label = "anonymous inverse dot applications",
        code =
          """a > namedA
            |  # testing anonymous
            |  # inverse-dot applications
            |  a.
            |    b.
            |      c.
            |        d""".stripMargin
      ),

      // TODO: here the information about inner names is lost
      TestCase(
        label = "named inverse-dot applications",
        code =
          """a > main
            |  a. > d-c-b-a
            |    b. > d-c-b
            |      c. > d-c
            |        d > d-""".stripMargin
      )
    )
    checkExamples(namedApplicationAllInput(_), correctExamples = correctExamples)
  }

  "arrays" should {

    val correctExamples: List[TestCase[EOProg[EOExprOnly]]] = List(
      TestCase(
        label = "simple single line array",
        code =
          """
            |* "Lucy" "Jeff" 314 > stuff
            |""".stripMargin,
      ),

      TestCase(
        label = "nested single line array",
        code =
          """
            |* (* deep stuff here) 'a' 'b' 'c' > some_deep_stuff
            |
            |""".stripMargin
      ),

      TestCase(
        label = "simple multiline array",
        code =
          """
            |*
            |  "hello"
            |  "world"
            |  'I'
            |  "am"
            |  "array"
            |  "I have no name"
            |""".stripMargin,
      ),
      TestCase(
        label = "nested multiline array",
        code =
          """
            |* > cool
            |  "hello"
            |  "world"
            |  *
            |    'I'
            |    "am"
            |    "array"
            |  *
            |    *
            |      "My name is cool"
            |      *
            |        *
            |          literally
            |""".stripMargin,
      )
    )

    checkExamples(programParser, correctExamples)
  }
}
