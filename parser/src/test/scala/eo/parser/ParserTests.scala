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
    """
      |+package sandbox
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
      |
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
  def producesParserError(result: ParserResult): Boolean = {
    result match {
      case Left(error) =>
        error match {
          case LexerError(_) => false
          case ParserError(_) => true
        }
      case Right(_) => false
    }
  }
  def producesLexerError(result: ParserResult): Boolean = {
    result match {
      case Left(error) =>
        error match {
          case LexerError(_) => true
          case ParserError(_) => false
        }
      case Right(_) => false
    }
  }

  describe("Parser") {
    describe("produces correct AST for correct programs") {
      it("mutual recursion example") {
        assert(Parser(MutualRecExample.code) == Right(MutualRecExample.ast))
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
