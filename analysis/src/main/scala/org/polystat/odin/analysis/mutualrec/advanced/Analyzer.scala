package org.polystat.odin.analysis.mutualrec.advanced

import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Sync}
import fs2.Stream
import cats.Eq
import org.polystat.odin.analysis.ASTAnalyzer
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.core.ast.EOProg
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.EoParser.sourceCodeEoParser
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._

object Analyzer {
  // build a tree
  // convert a tree into Program

  sealed trait Tree[A] {
    val node: A
    def find(predicate: A => Boolean): Option[A]
  }

  def untilDefined[A, B](lst: List[A])(f: A => Option[B]): Option[B] =
    lst match {
      case Nil => None
      case head :: tail => f(head).orElse(untilDefined(tail)(f))
    }

  sealed case class Branch[A](node: A, children: List[Tree[A]])
    extends Tree[A] {

    override def find(predicate: A => Boolean): Option[A] =
      if (predicate(node))
        Some(node)
      else
        untilDefined(children)(t => t.find(predicate))

  }

  sealed case class Leaf[A](node: A) extends Tree[A] {

    override def find(predicate: A => Boolean): Option[A] =
      Option.when(predicate(node))(node)

  }

//  [] > a
//    [] > aa
  //    [self] > g
  //      self > @
//    a.a > @
//
//  [] > b

// val aboba: Tree[Int] = Tree(1, List(Fix(Tree(1, List(Fix(Tree(1,
  // List())))))))
//
//  val tree: List[Tree[(Object, Option[ObjectName])]] =
//    List(
//      Tree(
//        (
//          Object(
//            name = ObjectName(None, "a"),
//            parent = None,
//            nestedObjs = List(),
//            callGraph = ???
//          ),
//            Some(ObjectName(None, "b")
//        ),
//        List(Fix(Tree((ObjectName(None, "aa"), None), List()))),
//      ),
//      Tree(
//        (ObjectName(None, "b"), None),
//        List()
//      )
//    )

  /* EOProg -> Program by using object signature: [] > name
   * 1. filter EOProg for object signatures
   * 2. convert every matching EOBndExpr to Object
   * 3. wrap the result in Program */
  def findObjs(
    ast: EOProg[Fix[EOExpr]]
  ): Program = {
    /* EoBnd -> Object
     * 1. name comes from EOBnd.name
     * 2. parent comes from searching bndAttrs for Object with EO decoration
     * 3. nestedObjs come from recursively calling the function with the current
     * object as a container
     * 4. callGraph will be generated from the from EoObj */
    def bndHelper(
      container: Option[ObjectName]
    )(bnd: EOBnd[Fix[EOExpr]]): List[Object] =
      bnd match {
        case EOBndExpr(bndName, Fix(obj @ EOObj(Vector(), None, bndAttrs))) =>
          val objName = ObjectName(container, bndName.name.name)
          val currentObj = Object(
            name = objName,
            parent = getObjectParent(bndAttrs),
            nestedObjs = bndAttrs.flatMap(bndHelper(Some(objName))).toList,
            callGraph = getObjCallGraph(obj, objName)
          )
          List(currentObj)
        case EOBndExpr(_, Fix(EOObj(_, _, bndAttrs))) =>
          bndAttrs.flatMap(bndHelper(container)).toList
        case _ => List()
      }
    Program(ast.bnds.flatMap(bndHelper(None)).toList)
  }

  // The target object can be either EoSimpleApp or EODot
  // TODO: make it so that it can set ALL the parents
  def getObjectParent(
    bndAttrs: Vector[EOBndExpr[Fix[EOExpr]]]
  ): Option[Object] = {

    // TODO: find a way to to this properly
    def createObj(name: String): Some[Object] = Some(
      Object(
        name = ObjectName(None, name),
        parent = None,
        nestedObjs = List(),
        callGraph = Map()
      )
    )

    bndAttrs
      .find {
        case EOBndExpr(EODecoration, Fix(EOSimpleApp(_))) => true
        case EOBndExpr(EODecoration, Fix(EODot(_, _))) => true
        case _ => false
      }
      .flatMap(_.expr match {
        case EOSimpleApp(name) => createObj(name)
        case EODot(_, name) => createObj(name)
      })
  }

  /* EOObj -> CallGraph CallGraph entry: MethodName -> Set[MethodName] method
   * call signature: self.name self params
   * 1. filter bndAttrs for method declaration signatures and extract their
   * method names,
   * 2. search method bodies for method call signatures */
  def getObjCallGraph(
    obj: EOObj[Fix[EOExpr]],
    objName: ObjectName
  ): CallGraph = {

    def getInnerCalls(obj: EOExpr[Fix[EOExpr]]): Set[MethodName] = obj match {
      case EOObj(_, _, bndAttrs) => bndAttrs
          .foldLeft(Set.empty[MethodName])((acc, el) =>
            el.expr match {
              case EOCopy(
                     EODot(EOSimpleApp("self"), name),
                     args
                   ) =>
                (acc + MethodName(objName, name)) ++ args
                  .value
                  .asInstanceOf[Vector[EOBnd[Fix[EOExpr]]]]
                  .flatMap((bnd: EOBnd[Fix[EOExpr]]) =>
                    getInnerCalls(Fix.un(bnd.expr))
                  )
              case _ => acc
            }
          )
      case EOCopy(Fix(trg), args) => getInnerCalls(trg).union(
          args.flatMap(bnd => getInnerCalls(Fix.un(bnd.expr))).toSet
        )
      case _ => Set()
    }

    def getObjMethods(obj: EOObj[Fix[EOExpr]]): List[EOBndExpr[Fix[EOExpr]]] =
      obj.bndAttrs.foldLeft[List[EOBndExpr[Fix[EOExpr]]]](List.empty) {
        case (acc, bnd) =>
          bnd match {
            case method @ EOBndExpr(_, Fix(obj: EOObj[Fix[EOExpr]])) =>
              obj match {
                case EOObj(LazyName("self") +: _, _, _) =>
                  acc ++ List(method)
                case _ => acc
              }
            case _ => acc
          }
      }

    def accCallGraphEntry(
      acc: CallGraph,
      method: EOBndExpr[Fix[EOExpr]]
    ): CallGraph = {
      method.expr match {
        case Fix(obj @ EOObj(_, _, _)) =>
          val methodName = MethodName(objName, method.bndName.name.name)
          acc.updated(methodName, getInnerCalls(obj))
        case _ => acc
      }
    }

    getObjMethods(obj).foldLeft(Map.empty[MethodName, Set[MethodName]])(
      accCallGraphEntry
    )
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

    def ast[F[_]: Sync] = sourceCodeEoParser().parse(code)

    (for {
      ast <- ast[IO]
      _ <- IO.println(findObjs(ast).toEO)
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
