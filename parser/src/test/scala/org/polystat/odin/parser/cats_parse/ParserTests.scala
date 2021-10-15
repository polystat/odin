package org.polystat.odin.parser.cats_parse

import cats.parse.{Parser => P, Parser0 => P0}
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.EOParserTestSuite
import org.polystat.odin.parser.TestUtils.TestCase
import org.polystat.odin.parser.Gens

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
        Gens.emptyLinesOrComments
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
    }

    "chars" should {
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
      runParserTestsGen(Right(Tokens.identifier), Gens.identifier)
    }
  }

  "metas" should {
    "package meta" in {
      runParserTestsGen(Right(Metas.packageMeta), Gens.packageMeta)
    }

    "alias meta" in {
      runParserTestsGen(Right(Metas.aliasMeta), Gens.aliasMeta)
    }

    "rt meta" in {
      runParserTestsGen(Right(Metas.rtMeta), Gens.rtMeta)
    }

    "all metas" in {
      runParserTestsGen(Left(Metas.metas), Gens.metas)
    }
  }

  "abstraction params" should {
    "pass" in {
      runParserTestsGen(Right(SingleLine.params), Gens.abstractionParams)
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
      runParserTestsGen(Right(Named.name), Gens.bndName)
    }

    val incorrectTests = List[TestCase[EONamedBnd]](
      TestCase("incorrect symbol", " < name"),
      TestCase("no name", " > !"),
    )

    runParserTests[EONamedBnd](
      Right(Named.name),
      incorrectTests = incorrectTests
    )
  }

}

object ParserTests {

  def main(args: Array[String]): Unit = {
    for (_ <- 1 to 10) {
      println(Gens.abstractionParams.sample.get)
    }
  }

}
