package org.polystat.odin.parser

import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec
import TestUtils._
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly

trait EOParserTestSuite extends AnyWordSpec {

  type ParserT[A]
  type Success[A]
  type Error
  type ParserResultT[A] = Either[Error, Success[A]]

  def checkParser[A](
    check: ParserResultT[A] => Boolean
  )(parser: ParserT[A], input: String): Assertion

  def shouldFailParsing[A]: (ParserT[A], String) => Assertion =
    checkParser(_.isLeft)

  def shouldParse[A]: (ParserT[A], String) => Assertion = checkParser(_.isRight)

  def shouldProduceAST[AST](ast: AST): (ParserT[AST], String) => Assertion =
    checkParser {
      case Left(_) => false
      case Right(value) => value == ast
    }

  def programParser: ParserT[EOProg[EOExprOnly]]

  def singleLineApplicationParser: ParserT[EOExprOnly]

  type Tests[A] = List[TestCase[A]]

  def runTests[A](
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

  runTests(programParser, correctTests = examplesFromSources)

  runTests[EOExprOnly](
    singleLineApplicationParser,
    correctTests = SingleLineExamples.correct
  )

  runTests[EOProg[EOExprOnly]](
    programParser,
    correctTests = mutualRecursionExample
  )

}
