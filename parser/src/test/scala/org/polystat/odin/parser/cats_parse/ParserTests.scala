package org.polystat.odin.parser.cats_parse

import org.scalatest.wordspec.AnyWordSpec
import cats.parse.Parser0
import org.polystat.odin.parser.TestUtils.astPrinter
import org.scalatest.Assertion


class ParserTests extends AnyWordSpec {

  def shouldParse[A](parser: Parser0[A], input: String): Assertion = {
    val parsed = parser.parseAll(input)
    parsed match {
      case Left(value) => println(value)
      case Right(value) => astPrinter.pprintln(value)
    }
    assert(parsed.isRight)
  }

  "tokens" should {

    "comments or empty lines" in {
      shouldParse(Tokens.emptyLinesOrComments,
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
      shouldParse(Metas.packageMeta, "+package sandbox")
      shouldParse(Metas.packageMeta, "+package sandbox")
    }

    "alias meta" in {
      shouldParse(Metas.aliasMeta, "+alias biba boba")
      shouldParse(Metas.aliasMeta, "+alias stdout org.eolang.io.stdout")
    }

    "rt meta" in {
      shouldParse(Metas.rtMeta, "+rt jvm org.eolang:eo-runtime:0.1.24")
      shouldParse(Metas.rtMeta, "+rt   jvm\t  org.eolang:eo-runtime:0.1.24")
    }

    "all metas (with package)" in {
      shouldParse(Metas.metas,
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
      shouldParse(Metas.metas,
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
  }


}
