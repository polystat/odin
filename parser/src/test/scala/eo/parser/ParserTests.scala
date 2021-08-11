package eo.parser


import com.github.tarao.nonempty.collection.NonEmpty
import eo.core.ast.astparams.EOExprOnly
import eo.core.ast._
import higherkindness.droste.data.Fix
import org.scalatest.funspec.AnyFunSpec

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
      EONamedBnd(
        EOAnyName(LazyName("base")),
        Fix[EOExpr](
          EOObj(
            freeAttrs = Vector(),
            varargAttr = None,
            bndAttrs = Vector(
              EONamedBnd(
                EOAnyName(LazyName("x")),
                Fix[EOExpr](EOSimpleApp("memory"))
              ),
              EONamedBnd(
                EOAnyName(LazyName("f")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyName("self"), LazyName("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EONamedBnd(
                        EODecoration(),
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
              EONamedBnd(
                EOAnyName(LazyName("g")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyName("self"), LazyName("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EONamedBnd(
                        EODecoration(),
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


      EONamedBnd(
        EOAnyName(LazyName("derived")),
        Fix[EOExpr](
          EOObj(
            freeAttrs = Vector(),
            varargAttr = None,
            bndAttrs = Vector(
              EONamedBnd(EODecoration(), Fix[EOExpr](EOSimpleApp("base"))),
              EONamedBnd(
                EOAnyName(LazyName("f")),
                Fix[EOExpr](
                  EOObj(
                    freeAttrs = Vector(LazyName("self"), LazyName("v")),
                    varargAttr = None,
                    bndAttrs = Vector(
                      EONamedBnd(
                        EODecoration(),
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

  type ParserResult = Either[CompilationError, EOProg[EOExprOnly]]

  private def producesParserError(result: ParserResult): Boolean = {
    result match {
      case Left(error) =>
        error match {
          case LexerError(_) => false
          case ParserError(_) => true
        }
      case Right(_) => false
    }
  }

  private def producesLexerError(result: ParserResult): Boolean = {
    result match {
      case Left(error) =>
        error match {
          case LexerError(_) => true
          case ParserError(_) => false
        }
      case Right(_) => false
    }
  }

  private def assertCodeProducesAST(code: String, ast: Vector[EOBnd[EOExprOnly]]) = {
    assert(Parser(code) == Right(EOProg(EOMetas(None, Vector()), ast)))
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
            EONamedBnd(
              EOAnyName(LazyName("namedA")),
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
            EONamedBnd(
              EOAnyName(LazyName("aAppliedToBCandD")),
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
            Vector(EONamedBnd(
              EOAnyName(LazyName("rightAssociative")),
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
            EONamedBnd(
              EOAnyName(LazyName("leftAssociative")),
              Fix[EOExpr](EOCopy(
                Fix[EOExpr](EOCopy(
                  Fix[EOExpr](EOCopy(
                    Fix[EOExpr](EOSimpleApp("a")),
                    NonEmpty[Vector[EOBnd[EOExprOnly]]](EOAnonExpr(Fix[EOExpr](EOSimpleApp("b")))))),
                    NonEmpty[Vector[EOBnd[EOExprOnly]]](EOAnonExpr(Fix[EOExpr](EOSimpleApp("c")))))),
                  NonEmpty[Vector[EOBnd[EOExprOnly]]](EOAnonExpr(Fix[EOExpr](EOSimpleApp("d"))))
                )
                )
            )
          )
        )
      }

      it("parses mutual_rec_non_term.eo (without chained special attributes)") {
        import eo.backend.eolang.ToEO.instances._
        import eo.backend.eolang.ToEO.ops._
        import eo.backend.eolang.inlineorlines.ops._
        val code = {
          val src = io.Source.fromFile("core/src/test/resources/eo/mutual_rec_non_term.eo")
          try src.mkString finally src.close()
        }
        val ast = Parser(code)
        assert(ast match {
          case Left(_) => false
          case Right(_) => true
        })

        println(ast match {
          case Left(value) => println(s"ERROR: $value")
          case Right(value) => println(value.toEO.allLinesToString)
        })
      }
    }

    describe("produces errors for incorrect programs") {

      it("misplaced exclamation marks") {
        assert(
          producesParserError(
            Parser(FailingCode.misplacedExclamationMark)
          )
        )
      }

      it("invalid tokens") {
        assert(
          producesLexerError(
            Parser(FailingCode.invalidTokens)
          )
        )
      }
    }
  }


}
