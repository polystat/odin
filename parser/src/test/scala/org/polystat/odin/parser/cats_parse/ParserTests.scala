package org.polystat.odin.parser.cats_parse

import org.scalatest.wordspec.AnyWordSpec
import cats.parse.{Parser, Parser0}
import org.polystat.odin.parser.TestUtils.astPrinter
import org.scalatest.Assertion


class ParserTests extends AnyWordSpec {

  type ParserType[A] = Either[Parser0[A], Parser[A]]

  def checkParser[A](
                      check: Either[Parser.Error, A] => Boolean
                    )(
                      parser: ParserType[A],
                      input: String)
  : Assertion = {
    val parsed = parser match {
      case Left(value) => value.parseAll(input)
      case Right(value) => value.parseAll(input)
    }
    parsed match {
      case Left(value) => println(value)
      case Right(value) => astPrinter.pprintln(value)
    }
    assert(check(parsed))
  }

  def shouldParse[A]: (ParserType[A], String) => Assertion = {
    checkParser[A](_.isRight)
  }

  def shouldFailParsing[A]: (ParserType[A], String) => Assertion = {
    checkParser(_.isLeft)
  }

  "tokens" should {

    "comments or empty lines" in {
      shouldParse(Left(Tokens.emptyLinesOrComments),
        """
          |
          |
          |
          |
          |
          |  # 32434123
          |""".stripMargin)
    }

  }

  "metas" should {
    "package meta" in {
      shouldParse(Right(Metas.packageMeta), "+package sandbox")
      shouldParse(Right(Metas.packageMeta), "+package sandbox")
    }

    "alias meta" in {
      shouldParse(Right(Metas.aliasMeta), "+alias biba boba")
      shouldParse(Right(Metas.aliasMeta), "+alias stdout org.eolang.io.stdout")
    }

    "rt meta" in {
      shouldParse(Right(Metas.rtMeta), "+rt jvm org.eolang:eo-runtime:0.1.24")
      shouldParse(Right(Metas.rtMeta), "+rt   jvm\t  org.eolang:eo-runtime:0.1.24")
    }

    "all metas (with package)" in {
      shouldParse(Left(Metas.metas),
        """
          |
          |+package sandbox
          |+rt jvm org.eolang:eo-runtime:0.1.24
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
          |""".stripMargin
      )
    }

    "all metas (no package)" in {
      shouldParse(Left(Metas.metas),
        """+rt jvm org.eolang:eo-runtime:0.1.24
          |# alias meta
          |  # used to rename imported artifacts
          |+alias stdout org.eolang.io.stdout
          |+alias sprintf org.eolang.txt.sprintf
          |
          |+alias biba boba
          |""".stripMargin
      )
    }

    "just package" in {
      shouldParse(Left(Metas.metas),
        """
          |# package meta
          |+package sandbox
          |""".stripMargin
      )
    }

    "no metas" in {
      shouldParse(Left(Metas.metas),
        """
          |
          | # no metas here
          |""".stripMargin)
    }

    "nothing" in {
      shouldParse(Left(Metas.metas), "")
    }
  }

  "params" should {
    "empty" in {
      shouldParse(Right(SingleLine.params), "[]")
    }
    "no varargs" in {
      shouldParse(Right(SingleLine.params), "[a-a b c]")
    }
    "only phi" in {
      shouldParse(Right(SingleLine.params), "[@]")
    }
    "varargs" in {
      shouldParse(Right(SingleLine.params), "[a b c...]")
    }
    "vararg phi" in {
      shouldParse(Right(SingleLine.params), "[@...]")
    }
    "fail" in {
      shouldFailParsing(Right(SingleLine.params), "[...]")
      shouldFailParsing(Right(SingleLine.params), "[")
      shouldFailParsing(Right(SingleLine.params), "[a..]")
      shouldFailParsing(Right(SingleLine.params), "[a a a a a a a..]")
      shouldFailParsing(Right(SingleLine.params), "[a  ...]")
    }
  }


}
