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
  // build a tree
  // convert a tree into Program

  val exampleEO: String =
    """[] > a
      |  b > @
      |  [self] > f
      |    self > @
      |[] > b
      |  [] > d
      |    c > @
      |  [self] > g
      |    self > @
      |[] > c
      |  [] > e
      |    a > @
      |    [self] > h
      |      self.f self > @
      |[] > t
      |  c.e > @
      |  [self] > f
      |    self.h self > @
      |""".stripMargin

  case class ObjectInfo(
    parent: Option[ObjectName],
    methods: Vector[Method],
    nestedObjects: Vector[NestedObject]
  )

  type Method = (String, EOObj[EOExprOnly])
  type NestedObject = (String, EOObj[EOExprOnly])

  def splitObjectBody(
    body: Vector[EOBnd[EOExprOnly]]
  ): ObjectInfo =
    body.foldLeft(ObjectInfo(None, Vector.empty, Vector.empty)) {
      case (acc, next) => next match {
          case EOBndExpr(
                 EODecoration,
                 Fix(EOSimpleApp(name))
               ) => acc.copy(parent = Some(ObjectName(None, name)))
          case EOBndExpr(EODecoration, Fix(obj: EODot[EOExprOnly])) =>
            acc.copy(parent = eoDotToObjectName(obj))

          case EOBndExpr(
                 EOAnyNameBnd(LazyName(name)),
                 Fix(
                   EOObj(
                     Vector(),
                     None,
                     bnds: Vector[EOBndExpr[EOExprOnly]]
                   )
                 )
               ) => acc.copy(nestedObjects =
              acc.nestedObjects.appended((name, EOObj(Vector(), None, bnds)))
            )

          case EOBndExpr(
                 EOAnyNameBnd(LazyName(name)),
                 Fix(
                   EOObj(
                     params @ LazyName("self") +: _,
                     vararg,
                     bnds: Vector[EOBndExpr[EOExprOnly]]
                   )
                 )
               ) => acc.copy(methods =
              acc.methods.appended((name, EOObj(params, vararg, bnds)))
            )
          case _ => acc
        }
    }

  def eoDotToObjectName(eoDot: EODot[EOExprOnly]): Option[ObjectName] =
    eoDot match {
      case EODot(EOSimpleApp(obj), attr) =>
        Some(ObjectName(Some(ObjectName(None, obj)), attr))
      case EODot(Fix(dot: EODot[EOExprOnly]), name) => eoDotToObjectName(dot)
          .map(container => ObjectName(Some(container), name))
      case _ => None
    }

  def extractCalls(
    body: Vector[EOExpr[EOExprOnly]]
  )(getContainer: String => ObjectName): Set[MethodName] =
    body.foldLeft(Set.empty[MethodName]) { case (acc, next) =>
      next match {
        case EOCopy(
               Fix(EODot(Fix(EOSimpleApp("self")), name)),
               args
             ) =>
          args
            .value
            .headOption
            .map(bnd => Fix.un(bnd.expr))
            .fold(acc) {
              case EOSimpleApp("self") =>
                acc + MethodName(getContainer(name), name)
              case _ => acc
            }
            .union(
              extractCalls(args.tail.map(bnd => Fix.un(bnd.expr)))(getContainer)
            )
        case EOCopy(Fix(trg), args) =>
          acc
            .union(extractCalls(Vector(trg))(getContainer))
            .union(
              extractCalls(
                args.value.map(bnd => Fix.un(bnd.expr))
              )(getContainer)
            )
        case EODot(Fix(trg), _) =>
          acc.union(extractCalls(Vector(trg))(getContainer))
        case _ => acc
      }
    }

  def extractCallGraph(
    parent: Option[ObjectName],
    container: ObjectName
  )(methods: Vector[Method]): CallGraph = {
    def extractCallGraphEntry(method: Method): CallGraphEntry =
      (
        MethodName(container, method._1),
        extractCalls(method._2.bndAttrs.map(bnd => Fix.un(bnd.expr))) { name =>
          methods
            .find(_._1 == name)
            .fold(
              parent.getOrElse(
                throw new Exception(
                  "This object calls a method that's not defined!"
                )
              )
            )(_ => container)
        }
      )

    methods.map(extractCallGraphEntry).toMap

  }

  def buildTreeFromObj(container: Option[ObjectName])(
    obj: NestedObject
  ): Tree[(Object, Option[ObjectName])] = {

    val (name, body) = obj
    val bodyInfo = splitObjectBody(body.bndAttrs)
    val objectName = ObjectName(container, name)

    Tree(
      node = (
        Object(
          name = objectName,
          parent = None,
          nestedObjs = List(),
          callGraph =
            extractCallGraph(bodyInfo.parent, objectName)(bodyInfo.methods)
        ),
        bodyInfo.parent
      ),
      children =
        bodyInfo.nestedObjects.map(buildTreeFromObj(Some(objectName))).toList
    )
  }

  def buildTree(
    prog: EOProg[EOExprOnly]
  ): Vector[Tree[(Object, Option[ObjectName])]] =
    splitObjectBody(prog.bnds).nestedObjects.map(buildTreeFromObj(None))

  def resolveParent(
    progTree: Tree[(Object, Option[ObjectName])] // only for lookup
  )(parentName: ObjectName): Option[Object] = {
    progTree
      .find { case (obj, _) =>
        obj.name == parentName
      }
      .flatMap { case (parentObj, maybeParentOfParent) =>
        maybeParentOfParent match {
          case Some(parentOfParent) =>
            resolveParent(progTree)(parentOfParent).map(
              _.extended(parentObj.name, parentObj.callGraph)
            )
          case None => Some(parentObj)
        }
      }
  }

  def restoreObjectFromTree(
    progTree: Tree[(Object, Option[ObjectName])] // only for lookup
  )(tree: Tree[(Object, Option[ObjectName])]): Object = {
    val (partialObj, maybeParent) = tree.node
    maybeParent match {
      case Some(parent) => resolveParent(progTree)(parent).fold(
          throw new Exception(
            s"Parent object of ${partialObj.name.show} is specified, but not defined in the program"
          )
        )(parentObj =>
          parentObj.extended(partialObj.name, partialObj.callGraph)
        )
      case None => partialObj.copy(
          nestedObjs = tree.children.map(restoreObjectFromTree(progTree))
        )
    }
  }

  val dummyObj: Object = Object(
    name = ObjectName(None, "THIS NAME DOESN'T MATTER"),
    parent = None,
    nestedObjs = List(),
    callGraph = Map()
  )

  def buildProgram(
    trees: Vector[Tree[(Object, Option[ObjectName])]]
  ): Program = {
    Program(
      trees
        .map(restoreObjectFromTree(Tree((dummyObj, None), trees.toList)))
        .toList
    )
  }

  def main(args: Array[String]): Unit = {
    val code = exampleEO
    """
      |[self] > bebra
      |  seq > @
      |    self.amogus self
      |    self.aboba self
      |    self.dance
      |    self.correct self (self.zhat self) (self.zhrat self)
      |    1.add (self.fib self 2)
      |""".stripMargin
    (for {
      ast <- sourceCodeEoParser[IO]().parse(code)
//      _ <- IO.delay(pprint.pprintln(Fix.un(ast.bnds(0).expr)))
//      _ <- IO.println(
//        extractCallGraph(ObjectName(None, "sasamba"))(
//          Vector(
// ("bebra", Fix.un(ast.bnds(0).expr).asInstanceOf[EOObj[EOExprOnly]])
//          )
//        ).show
//      )'
      _ <- IO.println(buildProgram(buildTree(ast)).objs.flatMap(_.callGraph.findCycles.map(_.show)).mkString("\n"))
    } yield ()).unsafeRunSync()

  }

  def untilDefined[A, B](lst: List[A])(f: A => Option[B]): Option[B] =
    lst match {
      case Nil => None
      case head :: tail => f(head).orElse(untilDefined(tail)(f))
    }

  sealed case class Tree[A](node: A, children: List[Tree[A]]) {

    def find(predicate: A => Boolean): Option[A] =
      if (predicate(node))
        Some(node)
      else
        untilDefined(children)(_.find(predicate))

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
                  .flatMap(bnd => getInnerCalls(Fix.un(bnd.expr)))
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
