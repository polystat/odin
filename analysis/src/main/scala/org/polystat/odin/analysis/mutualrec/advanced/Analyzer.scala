package org.polystat.odin.analysis.mutualrec.advanced

import cats.effect.unsafe.implicits.global
import cats.effect.IO
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
    """# v.z -> w.q -> v.z
      |# w.a -> v.z -> w.q -> v.z
      |# w.s -> v.z -> w.q -> v.z
      |# w.q -> v.z -> w.q
      |
      |[] > e
      |  [self] > m
      |    self.s self > @
      |  [self] > s
      |    self > @
      |  [self] > v
      |    self.s self > @
      |
      |
      |[] > v
      |  [self] > z
      |    self.q self > @
      |  [self] > q
      |    self > @
      |
      |
      |[] > w
      |  v > @
      |  [self] > a
      |    self.z self > @
      |  [self] > s
      |    self.z self > @
      |  [self] > q
      |    self.z self > @
      |
      |
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
  ): Program = Program(
    trees
      .map(restoreObjectFromTree(Tree((dummyObj, None), trees.toList)))
      .toList
  )

  def findErrors(prog: Program): List[OdinAnalysisError] =
    prog.findCycles.map(cc => OdinAnalysisError(cc.show))

  def analyzeAst(prog: EOProg[EOExprOnly]): List[OdinAnalysisError] =
    findErrors(buildProgram(buildTree(prog)))

  def main(args: Array[String]): Unit = {
    val code = exampleEO
    (for {
      ast <- sourceCodeEoParser[IO]().parse(code)
      _ <- advancedMutualRecursionAnalyzer.analyze(ast).evalMap(IO.println).compile.drain
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

  // TODO: this should be explicit
  implicit def advancedMutualRecursionAnalyzer[F[_]]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] = for {
        error <- Stream.emits(analyzeAst(ast))
      } yield error

    }

}
