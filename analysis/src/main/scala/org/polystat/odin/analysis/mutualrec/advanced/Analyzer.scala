package org.polystat.odin.analysis.mutualrec.advanced

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import fs2.Stream
import org.polystat.odin.analysis.ASTAnalyzer
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.core.ast.EOProg
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._

object Analyzer {
  /* EOProg -> Program object signature: [] > name
   * 1. filter EOProg for object signatures
   * 2. map this list with a function that converts EOBnds to Objects
   * 3. wrap the result in Program */

  /* EoBnd -> Object
   * 1. Object.name will come from EOBnd.name
   * 2. parent will come from searching bndAttrs for Object with EO decoration
   * 2.1. This object can be either EoSimpleApp or EODot EOBndExpr(EODecoration,
   * Fix[EOExpr](EOSimpleApp("base")))
   * 3. nestedObjs will come from recursively calling this function with the
   * current object as a container
   * 3.1. may need a helper
   * 4. callGraph will come from a function that generates a call graph from
   * EoObj */

  /* EOObj -> CallGraph CallGraph entry: MethodName -> Set[MethodName] method
   * call signature: self.name self params
   * 1. filter bndAttrs for method declaration signatures, extract method names
   * from them
   * 2. search method bodies for method call signatures
   * 3. compose them together */

  def findObjs(
    ast: EOProg[Fix[EOExpr]]
  ): List[EOBndExpr[Fix[EOExpr]]] = {
    def bndHelper(
      bnd: EOBnd[Fix[EOExpr]],
      container: Option[Object]
    ): List[EOBndExpr[Fix[EOExpr]]] =
      bnd match {
        case EOBndExpr(bndName, Fix(obj @ EOObj(Vector(), None, bndAttrs))) =>
          Object(
            name = ObjectName(container.map(_.name), bndName.name.name),
            parent = ???, // search bndAttrs for Object with EO decoration
            nestedObjs = ???,
            /* filter bndAttrs for other objects and call this function
             * recursively */
            callGraph = getObjCallGraph(obj)
          )
        case EOBndExpr(bndName, Fix(EOObj(_, _, bndAttrs))) =>
          bndAttrs.flatMap(bndHelper).toList
        case _ => List()
      }

    ast.bnds.flatMap(bndHelper).toList
  }

  def getObjCallGraph(
    obj: EOObj[Fix[EOExpr]]
  ): CallGraph = {
    def getCallGraphEntry(method: EOObj[Fix[EOExpr]]): CallGraphEntry = ???
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |+package sandbox
        |+alias stdout org.eolang.io.stdout
        |+alias sprintf org.eolang.txt.sprintf
        |
        |[p cont] > assert
        |  if. > @
        |    p
        |    false
        |    cont
        |
        |[] > character
        |
        |  [] > hui
        |    character > @
        |   
        |  [self mana] > checkMana
        |    0 > minMana
        |    seq > @
        |      stdout "Checking character's mana\n"
        |      assert (mana.less minMana) true
        |  [self mana] > castSpell
        |    seq > @
        |      self.checkMana self (mana.add(100))
        |      stdout
        |        sprintf
        |          "Character with %d mana casts a spell\n"
        |          mana
        |  [self mana] > attack
        |    seq > @
        |      stdout
        |        sprintf
        |          "Character with %d mana attacks\n"
        |          mana
        |
        |[] > god
        |  character > @
        |  [self mana] > checkMana
        |    seq > @
        |      stdout
        |        sprintf "The God has unlimited mana\n"
        |  [self mana] > attack
        |    self.castSpell self mana > @
        |
        |[args...] > app
        |  memory > mana
        |  character > regular_character
        |  god > god_character
        |  seq > @
        |    mana.write 10
        |    stdout
        |      sprintf "Regular character:\n"
        |    regular_character.castSpell regular_character mana
        |    mana.write (mana.sub 5)
        |    regular_character.attack regular_character mana
        |    stdout
        |      sprintf "\nGod character:\n"
        |    god_character.castSpell god_character mana
        |    god_character.attack god_character mana
        |    0
        |
        |""".stripMargin

    def ast[F[_]: Sync] = sourceCodeEoParser(2).parse(code)

    (for {
      ast <- ast[IO]
      _ <- IO.println(findObjs(ast).map(_.bndName.name))
    } yield ()).unsafeRunSync()
  }

  // TODO: this should be explicit
  implicit def advancedMutualRecursionAnalyzer[F[_]: Sync]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] = for {
        error <- Stream.eval(Sync[F].pure(OdinAnalysisError("aboba")))
      } yield error

    }

}
