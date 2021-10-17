package org.polystat.odin.parser

import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec
import TestUtils._
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.scalacheck.{Gen, Prop, Test}
import org.scalatestplus.scalacheck.Checkers

trait EOParserTestSuite extends AnyWordSpec with Checkers {

  private val numberOfTests = 10000

  val scalacheckParams: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(numberOfTests)
    .withMaxSize(numberOfTests)
    .withMinSize(numberOfTests)
    .withWorkers(8)
    .withTestCallback(new Test.TestCallback {

      override def onTestResult(name: String, result: Test.Result): Unit = {
        println(
          s"""
             |Finished with
             |Status: ${result.status}
             |Tests passed: ${result.succeeded}
             |Tests discarded: ${result.discarded}
             |Time: ${result.time}ms
             |""".stripMargin
        )
      }

    })
    .withLegacyShrinking(false)

  implicit def bool2Assertion(b: Boolean): Assertion = {
    assert(b)
  }

  type ParserT[A]
  type Success[A]
  type Error
  type ParserResultT[A] = Either[Error, Success[A]]

  def checkParser[A](
    check: ParserResultT[A] => Boolean
  )(parser: ParserT[A], input: String): Boolean

  def shouldFailParsing[A]: (ParserT[A], String) => Boolean =
    checkParser(_.isLeft)

  def shouldParse[A]: (ParserT[A], String) => Boolean = checkParser(_.isRight)

  def shouldProduceAST[AST](ast: AST): (ParserT[AST], String) => Boolean =
    checkParser {
      case Left(_) => false
      case Right(value) => value == ast
    }

  def programParser: ParserT[EOProg[EOExprOnly]]

  def singleLineApplicationParser: ParserT[EOExprOnly]

  type Tests[A] = List[TestCase[A]]

  def runParserTests[A](
    parser: ParserT[A],
    correctTests: Tests[A] = Nil,
    incorrectTests: Tests[A] = Nil
  ): Unit = {
    (
      (correctTests, shouldParse[A]) ::
        (incorrectTests, shouldFailParsing[A]) ::
        Nil
    ).foreach { case (examples, check) =>
      examples.foreach {
        case TestCase(label, code, Some(ast)) =>
          registerTest(label) {
            shouldProduceAST[A](ast)(parser, code)
          }
        case TestCase(label, code, None) =>
          registerTest(label) {
            check(parser, code)
          }
      }
    }
  }

  def runParserTestsGen[A](p: ParserT[A], gen: Gen[String]): Assertion = {
    check(
      Prop.forAll(gen) { generated =>
        shouldParse(p, generated)
      },
      scalacheckParams
    )
  }

  val examplesFromSources: Tests[EOProg[EOExprOnly]] = getListOfFiles(
    "/eo_sources"
  ).map(filename => TestCase(fileNameOf(filename), readCodeFrom(filename)))

  val mutualRecursionExample: Tests[EOProg[EOExprOnly]] = List(
    TestCase(
      "Mutual Recursion Example",
      MutualRecExample.code,
      Some(MutualRecExample.ast)
    )
  )

  "existing programs" should {
    runParserTests(programParser, correctTests = examplesFromSources)
    runParserTests(programParser, correctTests = mutualRecursionExample)

  }

  "single line application" should {
    runParserTests[EOExprOnly](
      singleLineApplicationParser,
      correctTests = SingleLineExamples.correct
    )
  }

}
