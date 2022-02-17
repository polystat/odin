package org.polystat.odin.parser.eo

import cats.effect.IO
import cats.implicits._
import cats.parse.{Parser => P, Parser0 => P0}
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.TestUtils.TestCase
import org.polystat.odin.utils.files
import org.polystat.odin.parser.gens._
import org.polystat.odin.parser.ast_tests._
import org.scalacheck.{Gen, Prop, Test}
import ParserTests._

abstract class ParserTests
  extends munit.CatsEffectSuite
     with munit.ScalaCheckSuite {

  override def scalaCheckTestParameters: Test.Parameters = Test
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

  def runParserTests[A](
    prefix: String,
    parser: ParserT[A],
    correctTests: IO[List[TestCase[A]]] = IO.pure(List()),
    incorrectTests: IO[List[TestCase[A]]] = IO.pure(List()),
  ): IO[Unit] = {
    List(
      (correctTests, shouldParse[A]),
      (incorrectTests, shouldFailParsing[A]),
    ).traverse_ { case (examples, check) =>
      examples.map(cases =>
        cases.foreach {
          case TestCase(label, code, Some(ast)) =>
            test(prefix + label)(shouldProduceAST[A](ast)(parser, code))
          case TestCase(label, code, None) =>
            test(prefix + label)(check(parser, code))
        }
      )
    }
  }

}

class TokenTests extends ParserTests {

  property("comments or empty lines") {
    runParserTestGen(
      Tokens.emptyLinesOrComments.asLeft,
      eo.emptyLinesOrComments
    )
  }

  property("strings - generated tests") {
    Prop.forAllNoShrink(eo.string) { string =>
      shouldParse(Tokens.string.asRight, string)
    }
  }

  val stringTests: List[TestCase[String]] = List(
    TestCase(
      label = "basic escapes",
      code =
        """"\nHello,\n\r\tthis is a 'char'\n\t\b\tand this is a \"string\"\n"""",
      ast = Some(
        "\nHello,\n\r\tthis is a 'char'\n\t\b\tand this is a \"string\"\n"
      )
    ),
    TestCase(
      label = "russian unicode",
      code =
        "\"\\u043F\\u0440\\u0438\\u0432\\u0435\\u0442\\u002C\\u0020\\u044F\\u0020\\u0027\\u0441\\u0438\\u043C" +
          "\\u0432\\u043E\\u043B\\u0027\\u002C\\u0020\\u0430\\u0020\\u044D\\u0442\\u043E\\u0020\\u0022\\u0441\\u0442" +
          "\\u0440\\u043E\\u0447\\u043A\\u0430\\u0022\"",
      ast = Some("привет, я 'символ', а это \"строчка\"")
    ),
    TestCase(
      label = "japanese unicode",
      code =
        "\"\\u60e3\\u6d41\\u00b7\\u660e\\u65e5\\u9999\\u00b7\\u5170\\u683c\\u96f7\"",
      ast = Some("惣流·明日香·兰格雷")
    )
  )

  runParserTests("strings - ", Tokens.string.asRight, stringTests.pure[IO])
    .unsafeRunSync()

  val charTests: List[TestCase[Char]] = List(
    TestCase(label = "new line", code = "\'\\n\'", Some('\n')),
    TestCase(label = "tab", code = "\'\\t\'", Some('\t')),
    TestCase(label = "A", code = "'A'", Some('A')),
    TestCase(label = "Ж (escaped)", code = "\'\\u0416\'", Some('Ж')),
    TestCase(label = "Ж (literal)", code = "'Ж'", Some('Ж')),
    TestCase(label = "香 (escaped)", code = "\'\\u9999\'", Some('香')),
  )

  runParserTests("chars - ", Tokens.char.asRight, charTests.pure[IO])

  property("chars - generated tests") {
    runParserTestGen(Tokens.char.asRight, eo.char)
  }

  property("identifiers") {
    Prop.forAllNoShrink(eo.identifier) { id =>
      shouldProduceAST[String](id)(Tokens.identifier.asRight, id)
    }
  }

  property("integers") {
    Prop.forAllNoShrink(eo.integer) { int =>
      shouldProduceAST[Int](int.toInt)(Tokens.integer.asRight, int)
    }
  }

  property("floats") {
    Prop.forAllNoShrink(eo.float) { float =>
      shouldProduceAST[Float](float.toFloat)(Tokens.float.asRight, float)
    }

  }

}

class MetasTests extends ParserTests {

  property("package meta") {
    runParserTestGen(Metas.aliasMeta.asRight, eo.aliasMeta)
  }

  property("alias meta") {
    runParserTestGen(Metas.aliasMeta.asRight, eo.aliasMeta)
  }

  property("rt meta") {
    runParserTestGen(Metas.rtMeta.asRight, eo.rtMeta)
  }

  property("all metas") {
    runParserTestGen(Metas.metas.asLeft, eo.metas)
  }

}

class AbstractionParamsTests extends ParserTests {

  property("should parse") {
    runParserTestGen(SingleLine.params.asRight, eo.abstractionParams)
  }

  test("should fail") {
    shouldFailParsing(Right(SingleLine.params), "[...]")
    shouldFailParsing(Right(SingleLine.params), "[")
    shouldFailParsing(Right(SingleLine.params), "[a..]")
    shouldFailParsing(Right(SingleLine.params), "[a a a a a a a..]")
    shouldFailParsing(Right(SingleLine.params), "[a  ...]")
  }

}

class BindingNameTests extends ParserTests {

  property("pass") {
    runParserTestGen(SingleLine.bndName.asRight, eo.bndName)
  }

  val incorrectTests: List[TestCase[EONamedBnd]] = List[TestCase[EONamedBnd]](
    TestCase("incorrect symbol", " < name"),
    TestCase("no name", " > !"),
  )

  runParserTests[EONamedBnd](
    "binding name should fail parsing - ",
    SingleLine.bndName.asRight,
    incorrectTests = incorrectTests.pure[IO]
  ).unsafeRunSync()

}

class ProgramTests extends ParserTests {

  property("single line application - generated tests") {
    runParserTestGen(
      singleLineApplicationParser,
      eo.singleLineApplication(maxDepth = 4)
    )
  }

  property("object - generated tests") {
    runParserTestGen(
      Right(Parser.`object`(0, 4)),
      eo.`object`(
        named = true,
        indentationStep = 4,
        maxDepth = 2
      )
    )
  }

  property("program - generated tests") {
    runParserTestGen(
      Left(Parser.program(0, indentationStep = 2)),
      eo.program(indentationStep = 2, maxDepth = 2)
    )
  }

  property("program - prog->pretty == prog->pretty->parsed->pretty") {
    Prop.forAll(ast.eoProg(4)) { prog =>
      val expected: Either[String, String] = Right(prog.toEOPretty)
      val actual: Either[String, String] =
        Parser.parse(prog.toEOPretty).map(_.toEOPretty)
      assertEquals(actual, expected)
    }
  }

  runParserTests(
    "hand-crafted examples - ",
    programParser,
    correctTests = examplesFromSources
  )
    .unsafeRunSync()

  runParserTests(
    "mutual recursion example - ",
    programParser,
    correctTests = IO.pure(FullProgramExamples.correct)
  ).unsafeRunSync()

  runParserTests(
    "single line examples - ",
    singleLineApplicationParser,
    correctTests = IO.pure(SingleLineExamples.correct)
  ).unsafeRunSync()

}

object ParserTests {

  private val numberOfTests = 10000

  type Success[A] = A
  type Error = P.Error
  type ParserT[A] = Either[P0[A], P[A]]
  type ParserResultT[A] = Either[Error, Success[A]]

  def programParser: ParserT[EOProg[EOExprOnly]] =
    Left(Parser.program(0, 2))

  def singleLineApplicationParser: ParserT[EOExprOnly] =
    Right(SingleLine.singleLineApplication)

  def checkParser[A](
    check: ParserResultT[A] => Boolean
  )(parser: ParserT[A], input: String): Boolean = {
    val parsed = parser match {
      case Left(value) => value.parseAll(input)
      case Right(value) => value.parseAll(input)
    }
    parsed match {
      case Left(value) =>
        println(new Prettyprint(input = input).prettyprint(value))
      case Right(_) => ()
    }
    check(parsed)
  }

  def runParserTestGen[A](parser: ParserT[A], gen: Gen[String]): Prop = {
    Prop.forAllNoShrink(gen) { it =>
      shouldParse(parser, it)
    }
  }

  def shouldFailParsing[A]: (ParserT[A], String) => Boolean =
    checkParser(_.isLeft)

  def shouldParse[A]: (ParserT[A], String) => Boolean = checkParser(_.isRight)

  def shouldProduceAST[AST](ast: AST): (ParserT[AST], String) => Boolean =
    checkParser {
      case Left(_) => false
      case Right(value) => value == ast
    }

  val examplesFromSources: IO[List[TestCase[EOProg[EOExprOnly]]]] =
    files
      .readEoCodeFromDirectory[IO]("/eo_sources")
      .map(
        _.map { case (name, code) =>
          TestCase(label = name, code = code)
        }
      )

}
