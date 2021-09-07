package eo.parser.scala_parser_combinators


import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast.astparams.EOExprOnly
import eo.core.ast._
import eo.parser.scala_parser_combinators.errors.{ ParsingError, LexerError, ParserError }
import higherkindness.droste.data.Fix
import org.scalatest.Inspectors.forAll
import org.scalatest.funspec.AnyFunSpec

import scala.reflect.ClassTag
import java.io.File


object MutualRecExample {
  val ast: EOProg[EOExprOnly] = EOProg(
    EOMetas(
      pack = Some("sandbox"),
      metas = Vector(
        EOAliasMeta("stdout", "org.eolang.io.stdout"),
        EOAliasMeta("sprintf", "org.eolang.txt.sprintf"),
      )
    ),
    Vector(
      EOBndExpr(
        EOAnyNameBnd(LazyName("base")),
        Fix[EOExpr](
          EOObj(
            freeAttrs = Vector(),
            varargAttr = None,
            bndAttrs = Vector(
              EOBndExpr(
                EOAnyNameBnd(LazyName("x")),
                Fix[EOExpr](EOSimpleApp("memory"))
              ),
              EOBndExpr(
                EOAnyNameBnd(LazyName("f")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyName("self"), LazyName("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EOBndExpr(
                        EODecoration,
                        Fix[EOExpr](
                          EOCopy(
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("x")), "write")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("v")))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              ),
              EOBndExpr(
                EOAnyNameBnd(LazyName("g")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyName("self"), LazyName("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EOBndExpr(
                        EODecoration,
                        Fix[EOExpr](
                          EOCopy(
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("self")), "f")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("self"))),
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("v")))
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ),


      EOBndExpr(
        EOAnyNameBnd(LazyName("derived")),
        Fix[EOExpr](
          EOObj(
            freeAttrs = Vector(),
            varargAttr = None,
            bndAttrs = Vector(
              EOBndExpr(EODecoration, Fix[EOExpr](EOSimpleApp("base"))),
              EOBndExpr(
                EOAnyNameBnd(LazyName("f")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyName("self"), LazyName("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EOBndExpr(
                        EODecoration,
                        Fix[EOExpr](
                          EOCopy(
                            Fix[EOExpr](EODot(Fix[EOExpr](EOSimpleApp("self")), "g")),
                            NonEmpty[Vector[EOBnd[EOExprOnly]]](
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("self"))),
                              EOAnonExpr(Fix[EOExpr](EOSimpleApp("v")))
                            )
                          )
                        )
                      )
                    ),
                  )
                )
              )
            )
          )
        )
      ),
    )
  )
  val code: String =
    """+package sandbox
      |+alias stdout org.eolang.io.stdout
      |+alias sprintf org.eolang.txt.sprintf
      |[] > base
      |  memory > x
      |  [self v] > f
      |    x.write > @
      |      v
      |  [self v] > g
      |    self.f > @
      |      self
      |      v
      |[] > derived
      |  base > @
      |  [self v] > f
      |    self.g > @
      |      self
      |      v
      |""".stripMargin
}

object FailingCode {
  val misplacedExclamationMark: String =
    """
      |this
      |  is > wrooooong!!!!!!
      |""".stripMargin

  val invalidTokens: String =
    """
      |&~
      |""".stripMargin
}

class ParserTests extends AnyFunSpec {

  type ParserResult = Either[ParsingError, EOProg[EOExprOnly]]

  private def produces[A <: ParsingError : ClassTag](result: ParserResult): Boolean = {
    result match {
      case Left(_: A) => true
      case _ => false
    }
  }

  private def assertCodeProducesAST(code: String, ast: Vector[EOBnd[EOExprOnly]]) = {
    assert(Parser(code) == Right(EOProg(EOMetas(None, Vector()), ast)))
  }


  private def readCodeFrom(fileName: String): String = {
    val code = io.Source.fromFile(fileName)
    try code.mkString finally code.close()
  }

  private def getListOfFiles(dir: String): List[String] = {
    val file = new File(dir)
    file.listFiles().map(_.getPath).toList
  }


  describe("Parser") {
    describe("produces correct AST for correct programs") {
      it("mutual recursion example") {
        assert(Parser(MutualRecExample.code) == Right(MutualRecExample.ast))
      }

      it("single line application examples") {
        assertCodeProducesAST(
          code =
            """
              |a
              |""".stripMargin,

          ast = Vector(
            EOAnonExpr(Fix[EOExpr](EOSimpleApp("a")))
          )
        )
        assertCodeProducesAST(
          code =
            """
              |a > namedA
              |""".stripMargin,
          ast = Vector(
            EOBndExpr(
              EOAnyNameBnd(LazyName("namedA")),
              Fix[EOExpr](EOSimpleApp("a"))
            )
          )
        )
        assertCodeProducesAST(
          code =
            """
              |a b c d > aAppliedToBCandD
              |""".stripMargin,
          Vector(
            EOBndExpr(
              EOAnyNameBnd(LazyName("aAppliedToBCandD")),
              Fix[EOExpr](EOCopy(
                Fix[EOExpr](EOSimpleApp("a")),
                NonEmpty[Vector[EOBnd[EOExprOnly]]](
                  EOAnonExpr(Fix[EOExpr](EOSimpleApp("b"))),
                  EOAnonExpr(Fix[EOExpr](EOSimpleApp("c"))),
                  EOAnonExpr(Fix[EOExpr](EOSimpleApp("d")))
                )
              )
              )
            )
          )
        )
        assertCodeProducesAST(
          code =
            """
              |a (b (c d)) > rightAssociative
              |""".stripMargin,
          ast =
            Vector(EOBndExpr(
              EOAnyNameBnd(LazyName("rightAssociative")),
              Fix[EOExpr](EOCopy(
                Fix[EOExpr](EOSimpleApp("a")),
                NonEmpty[Vector[EOBnd[EOExprOnly]]](
                  EOAnonExpr(Fix[EOExpr](EOCopy(Fix[EOExpr](EOSimpleApp("b")),
                    NonEmpty[Vector[EOBnd[EOExprOnly]]](
                      EOAnonExpr(Fix[EOExpr](EOCopy(
                        Fix[EOExpr](EOSimpleApp("c")),
                        NonEmpty[Vector[EOBnd[EOExprOnly]]](
                          EOAnonExpr(Fix[EOExpr](EOSimpleApp("d")))))))))))))))
            )
        )
        assertCodeProducesAST(
          code =
            """
              |((a b) c) d > leftAssociative
              |""".stripMargin,
          ast = Vector(
            EOBndExpr(
              EOAnyNameBnd(LazyName("leftAssociative")),
              Fix[EOExpr](EOCopy(
                Fix[EOExpr](EOCopy(
                  Fix[EOExpr](EOCopy(
                    Fix[EOExpr](EOSimpleApp("a")),
                    NonEmpty[Vector[EOBnd[EOExprOnly]]](EOAnonExpr(Fix[EOExpr](EOSimpleApp("b")))))),
                  NonEmpty[Vector[EOBnd[EOExprOnly]]](EOAnonExpr(Fix[EOExpr](EOSimpleApp("c")))))),
                NonEmpty[Vector[EOBnd[EOExprOnly]]](EOAnonExpr(Fix[EOExpr](EOSimpleApp("d"))))
              ))
            )
          )
        )
      }

      forAll(getListOfFiles("core/src/test/resources/eo")) {
        (src: String) =>
          it(s"$src") {
            val ast = Parser(readCodeFrom(src))
            assert(ast.isRight)
          }
      }
    }


    describe("produces errors for incorrect programs") {

      it("misplaced exclamation marks") {
        assert(
          produces[ParserError](
            Parser(FailingCode.misplacedExclamationMark)
          )
        )
      }

      it("invalid tokens") {
        assert(
          produces[LexerError](
            Parser(FailingCode.invalidTokens)
          )
        )
      }
    }
  }

  describe("produces") {
    it("should return true if there is an error") {
      assert(produces[ParserError](Left(ParserError(""))))
      assert(produces[LexerError](Left(LexerError(""))))
    }

    it("should return false if the error is different") {
      assert(!produces[LexerError](Left(ParserError(""))))
      assert(!produces[ParserError](Left(LexerError(""))))
    }

    it("should return false if there is no error") {
      assert(!produces[LexerError](Right(EOProg(EOMetas(None, Vector()), Vector()))))
      assert(!produces[ParserError](Right(EOProg(EOMetas(None, Vector()), Vector()))))
    }
  }

}
