package org.polystat.odin.parser

import org.scalatest.Assertion
import org.scalatest.wordspec.AnyWordSpec
import TestUtils._
import org.polystat.odin.core.ast.EOProg
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.scalacheck.{Gen, Prop, Test}
import org.scalatestplus.scalacheck.Checkers
import EOParserTestSuite._

trait EOParserTestSuite extends AnyWordSpec with Checkers {

  type ParserT[A]
  type Success[A]
  type Error
  type ParserResultT[A] = Either[Error, Success[A]]

  implicit def bool2Assertion(b: Boolean): Assertion = {
    assert(b)
  }

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

object EOParserTestSuite {

  private val numberOfTests = 10000

  val scalacheckParams: Test.Parameters = Test
    .Parameters
    .default
    .withMinSuccessfulTests(numberOfTests)
    .withMaxSize(numberOfTests)
    .withMinSize(numberOfTests)
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

  val wspGen: Gen[String] =
    between(1, 3, Gen.oneOf(' ', '\t'))
      .map(_.mkString)

  val optWspGen: Gen[String] = between(0, 2, wspGen).map(_.mkString)
  val eolGen: Gen[String] = Gen.oneOf("\n", "\r\n")

  val smallLetterGen: Gen[Char] = Gen.alphaLowerChar
  val letterGen: Gen[Char] = Gen.alphaChar

  val emptyLinesOrCommentsGen: Gen[String] = {
    val emptyLine = for {
      wsp <- optWspGen
      eol <- eolGen
    } yield wsp + eol
    val comment = for {
      before <- optWspGen
      comment <- between(0, 15, Gen.asciiPrintableChar).map(_.mkString)
      eol <- eolGen
    } yield s"$before#$comment$eol"
    between(0, 3, Gen.oneOf(emptyLine, comment)).map(_.mkString)
  }

  def between[T](min: Int, max: Int, gen: Gen[T]): Gen[List[T]] = {
    for {
      len <- Gen.choose(min, max)
      gens <- Gen.listOfN(len, gen)
      lst <- gens
    } yield lst
  }

  def surroundedBy[T, S](gen: Gen[T], sur: Gen[S]): Gen[String] = for {
    before <- sur
    thing <- gen
    after <- sur
  } yield s"$before$thing$after"

  val digitGen: Gen[Char] = Gen.numChar

  val digitsGen: Gen[String] =
    between(1, 5, digitGen)
      .map(_.mkString)

  val integerGen: Gen[String] = for {
    sign <- Gen.frequency(
      (10, ""),
      (10, "-"),
      (1, "+")
    )
    num <- digitsGen
  } yield sign + num

  val identifierCharGen: Gen[Char] =
    Gen.frequency[Char](
      (5, smallLetterGen),
      (5, letterGen),
      (2, Gen.numChar),
      (1, '_'),
      (1, '-')
    )

  val identifierGen: Gen[String] = for {
    fst <- smallLetterGen
    rest <- between(0, 3, identifierCharGen)
  } yield (fst :: rest).mkString

  val packageNameGen: Gen[String] =
    between(1, 3, identifierGen)
      .map(_.mkString("."))

  val packageMetaGen: Gen[String] = for {
    name <- packageNameGen
    wsp <- wspGen
  } yield s"+package$wsp$name"

  val aliasMetaGen: Gen[String] = for {
    alias <- surroundedBy(identifierGen, wspGen)
    pkg <- packageNameGen
  } yield s"+alias$alias$pkg"

  val artifactNameGen: Gen[String] = for {
    pkgName <- packageNameGen
    artifactName <- identifierGen
    version <- between(3, 3, digitsGen.suchThat(s => !s.startsWith("0")))
      .map(_.mkString("."))
  } yield s"$pkgName:$artifactName:$version"

  val rtMetaGen: Gen[String] = for {
    alias <- surroundedBy(identifierGen, wspGen)
    artifactId <- artifactNameGen
  } yield s"+rt$alias$artifactId"

  val metasGen: Gen[String] = for {
    header <- emptyLinesOrCommentsGen
    pkg <- between(0, 1, packageMetaGen).map(_.mkString)
    eol <- eolGen
    metas <- between(
      0,
      5,
      for {
        comments <- emptyLinesOrCommentsGen
        meta <- Gen.oneOf(rtMetaGen, aliasMetaGen)
        eol <- eolGen
      } yield comments + meta + eol
    ).map(_.mkString)
  } yield header + pkg + eol + metas

}
