package org.polystat.odin.parser.eo

import cats.parse.{Parser => P, Parser0 => P0}
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EOParserTestSuite
import org.polystat.odin.parser.TestUtils.TestCase
import org.polystat.odin.parser.gens.eo

class ParserTests extends EOParserTestSuite {

  override type Success[A] = A
  override type Error = P.Error

  override def programParser: ParserT[EOProg[EOExprOnly]] =
    Left(Parser.program(0, 2))

  override def singleLineApplicationParser: ParserT[EOExprOnly] =
    Right(SingleLine.singleLineApplication)

  type ParserT[A] = Either[P0[A], P[A]]

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

  "tokens" should {

    "comments or empty lines" in {
      runParserTestsGen(
        Left(Tokens.emptyLinesOrComments),
        eo.emptyLinesOrComments
      )
    }

    "strings" should {
      val stringTests = List(
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
      runParserTests(Right(Tokens.string), stringTests)

      "pass" in {
        runParserTestsGen(Right(Tokens.string), eo.string)
      }
    }

    "chars" should {
      "pass" in {
        runParserTestsGen(Right(Tokens.char), eo.char)
      }
      val charTests = List(
        TestCase(label = "new line", code = "\'\\n\'", Some('\n')),
        TestCase(label = "tab", code = "\'\\t\'", Some('\t')),
        TestCase(label = "A", code = "'A'", Some('A')),
        TestCase(label = "Ж (escaped)", code = "\'\\u0416\'", Some('Ж')),
        TestCase(label = "Ж (literal)", code = "'Ж'", Some('Ж')),
        TestCase(label = "香 (escaped)", code = "\'\\u9999\'", Some('香')),
      )
      runParserTests(Right(Tokens.char), charTests)
    }

    "identifiers" in {
      runParserTestsGen(Right(Tokens.identifier), eo.identifier)
    }

    "integers" in {
      runParserTestsGen(Right(Tokens.integer), eo.integer)
    }

    "floats" in {
      runParserTestsGen(Right(Tokens.float), eo.float)
    }
  }

  "metas" should {
    "package meta" in {
      runParserTestsGen(Right(Metas.packageMeta), eo.packageMeta)
    }

    "alias meta" in {
      runParserTestsGen(Right(Metas.aliasMeta), eo.aliasMeta)
    }

    "rt meta" in {
      runParserTestsGen(Right(Metas.rtMeta), eo.rtMeta)
    }

    "all metas" in {
      runParserTestsGen(Left(Metas.metas), eo.metas)
    }
  }

  "abstraction params" should {
    "pass" in {
      runParserTestsGen(Right(SingleLine.params), eo.abstractionParams)
    }
    "fail" in {
      shouldFailParsing(Right(SingleLine.params), "[...]")
      shouldFailParsing(Right(SingleLine.params), "[")
      shouldFailParsing(Right(SingleLine.params), "[a..]")
      shouldFailParsing(Right(SingleLine.params), "[a a a a a a a..]")
      shouldFailParsing(Right(SingleLine.params), "[a  ...]")
    }
  }

  "binding name" should {
    "pass" in {
      runParserTestsGen(Right(SingleLine.bndName), eo.bndName)
    }

    val incorrectTests = List[TestCase[EONamedBnd]](
      TestCase("incorrect symbol", " < name"),
      TestCase("no name", " > !"),
    )

    runParserTests[EONamedBnd](
      Right(SingleLine.bndName),
      incorrectTests = incorrectTests
    )
  }

  "single line application" should {
    "pass" in {
      runParserTestsGen(
        singleLineApplicationParser,
        eo.singleLineApplication(maxDepth = 4)
      )
    }
  }

  "object" should {
    "pass" in {
      runParserTestsGen(
        Right(Parser.`object`(0, 4)),
        eo.`object`(
          named = true,
          indentationStep = 4,
          maxDepth = 4
        )
      )
    }
  }

  "program" should {
    "pass" in {
      runParserTestsGen(
        Left(Parser.program(0, indentationStep = 2)),
        eo.program(indentationStep = 2, maxDepth = 4)
      )
    }
  }

}

object ParserTests {

  import org.polystat.odin.backend.eolang.ToEO.ops._
  import org.polystat.odin.backend.eolang.ToEO.instances._

  def main(args: Array[String]): Unit = {
    val code =
      """
        |(([a b c] (1 > a)) 1 2 3) > aboba
        |
        |""".stripMargin

    val parsed = Parser.parse(code)

    parsed.foreach(pprint.pprintln(_))

    parsed match {
      case Left(value) => println(value)
      case Right(value) => println(value.toEOPretty)
    }

  }

}
