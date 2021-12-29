package org.polystat.odin.analysis.mutualrec.advanced

import cats.effect.{IO, Sync}
import cats.effect.unsafe.implicits.global
import cats.syntax.either._
import cats.Traverse
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

  type ErrorOr[A] = Either[String, A]

  val exampleEO: String =
    """# h.p.i.y -> v.h -> h.p.i.y
      |# v.h -> h.p.i.y -> v.h
      |
      |[] > h
      |  [self] > p
      |    self > @
      |  [self] > k
      |    self > @
      |  [self] > f
      |    self > @
      |  [self] > w
      |    self > @
      |
      |  [] > p
      |    [self] > o
      |      self > @
      |    [self] > h
      |      self > @
      |
      |    [] > i
      |      [self] > h
      |        self > @
      |      [self] > y
      |        self.h self > @
      |      [self] > e
      |        self > @
      |
      |
      |    [] > f
      |      [self] > s
      |        self > @
      |
      |
      |  [] > s
      |    [self] > b
      |      self > @
      |    [self] > l
      |      self.b self > @
      |
      |
      |  [] > x
      |    [self] > t
      |      self > @
      |
      |
      |  [] > n
      |    [self] > c
      |      self.e self > @
      |    [self] > f
      |      self > @
      |    [self] > e
      |      self.f self > @
      |
      |
      |[] > v
      |  h.p.i > @
      |  [self] > h
      |    self.y self > @
      |  [self] > e
      |    self > @
      |  [self] > o
      |    self > @
      |  [self] > q
      |    self.o self > @
      |
      |
      |""".stripMargin

  case class ObjectInfo(
    parent: Option[ObjectName],
    methods: Vector[Method],
    nestedObjects: Vector[NestedObject]
  )

  type Method = (
    String, // method name
    EOObj[EOExprOnly] // method body
  )

  type NestedObject = (
    String, // object name
    EOObj[EOExprOnly] // object body
  )

  type PartialObject = (
    Object, // an object containing the object name and partial call graph
    Option[ObjectName] // parent or decoratee
  )

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

  def splitObjectBody(
    body: Vector[EOBnd[EOExprOnly]]
  ): ObjectInfo =
    body.foldLeft(ObjectInfo(None, Vector.empty, Vector.empty)) {
      case (acc, next) => next match {

          // parent (simple app)
          // simple_name > @
          case EOBndExpr(
                 EODecoration,
                 Fix(EOSimpleApp(name))
               ) => acc.copy(parent = Some(ObjectName(None, name)))

          // parent (eo dot)
          // a.b.c > @
          case EOBndExpr(EODecoration, Fix(obj: EODot[EOExprOnly])) =>
            acc.copy(parent = eoDotToObjectName(obj))

          // object
          // [] > objName
          //   ...
          case EOBndExpr(
                 EOAnyNameBnd(LazyName(name)),
                 Fix(obj @ EOObj(Vector(), None, _))
               ) => acc.copy(nestedObjects =
              acc.nestedObjects.appended((name, obj))
            )

          // method
          // [self params] > methodName
          //   ...
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

          // any other binding that is not one of the above
          // 2 > two
          // 2.add 2 > four
          // etc.
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
  )(getContainer: String => ErrorOr[ObjectName]): ErrorOr[Set[MethodName]] =
    body.foldLeft[ErrorOr[Set[MethodName]]](Right(Set.empty)) {
      case (acc, next) =>
        next match {
          case EOCopy(
                 Fix(EODot(Fix(EOSimpleApp("self")), name)),
                 args
               ) =>
            for {
              accWithMethod <- args
                .headOption
                .map(bnd => Fix.un(bnd.expr))
                .fold(acc) {
                  case EOSimpleApp("self") =>
                    for {
                      container <- getContainer(name)
                      acc <- acc
                    } yield acc + MethodName(container, name)
                  case _ => acc
                }
              callsFromArgs <- extractCalls(
                args.tail.map(bnd => Fix.un(bnd.expr))
              )(getContainer)
            } yield accWithMethod.union(callsFromArgs)

          case EOCopy(Fix(trg), args) =>
            for {
              acc <- acc
              callsFromTrg <- extractCalls(Vector(trg))(getContainer)
              callsFromArgs <- extractCalls(
                args.value.map(bnd => Fix.un(bnd.expr))
              )(getContainer)

            } yield acc.union(callsFromTrg).union(callsFromArgs)
          case EODot(Fix(trg), _) =>
            for {
              acc <- acc
              callsFromTrg <- extractCalls(Vector(trg))(getContainer)
            } yield acc.union(callsFromTrg)
          case _ => acc
        }
    }

  def extractCallGraph(
    parent: Option[ObjectName],
    container: ObjectName
  )(methods: Vector[Method]): ErrorOr[CallGraph] = {
    def extractCallGraphEntry(method: Method): ErrorOr[CallGraphEntry] = {
      val (methodName, methodBody) = method
      for {
        calls <- extractCalls(
          methodBody.bndAttrs.map(bnd => Fix.un(bnd.expr))
        ) { name =>
          methods
            .find(_._1 == name)
            .fold(
              Either.fromOption(
                parent,
                "This object calls a method that's not defined!"
              )
            )(_ => Right(container))
        }
      } yield (
        MethodName(container, methodName),
        calls
      )
    }
    Traverse[Vector].sequence(methods.map(extractCallGraphEntry)).map(_.toMap)
  }

  def buildTreeFromObj(container: Option[ObjectName])(
    obj: NestedObject
  ): ErrorOr[Tree[PartialObject]] = {

    val (name, body) = obj
    val bodyInfo = splitObjectBody(body.bndAttrs)
    val objectName = ObjectName(container, name)

    for {
      cg <- extractCallGraph(bodyInfo.parent, objectName)(bodyInfo.methods)
      nestedObjs <- Traverse[Vector].sequence(
        bodyInfo.nestedObjects.map(buildTreeFromObj(Some(objectName)))
      )
    } yield Tree(
      node = (
        Object(
          name = objectName,
          parent = None,
          nestedObjs = List(),
          callGraph = cg
        ),
        bodyInfo.parent
      ),
      children = nestedObjs.toList
    )
  }

  def buildTree(
    prog: EOProg[EOExprOnly]
  ): ErrorOr[Vector[Tree[PartialObject]]] =
    Traverse[Vector].sequence(
      splitObjectBody(prog.bnds).nestedObjects.map(buildTreeFromObj(None))
    )

  def resolveParent(
    progTree: Tree[PartialObject] // only for lookup
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
    progTree: Tree[PartialObject] // only for lookup
  )(tree: Tree[PartialObject]): ErrorOr[Object] = {
    val (partialObj, maybeParent) = tree.node
    maybeParent match {
      case Some(parent) => Either
          .fromOption(
            resolveParent(progTree)(parent),
            s"Parent object of ${partialObj.name.show} is specified, but not defined in the program!"
          )
          .map(parentObj =>
            parentObj.extended(partialObj.name, partialObj.callGraph)
          )
      case None =>
        for {
          nestedObjs <- Traverse[List].sequence(
            tree.children.map(restoreObjectFromTree(progTree))
          )
        } yield partialObj.copy(
          nestedObjs = nestedObjs
        )
    }
  }

  def buildProgram(
    trees: Vector[Tree[PartialObject]]
  ): ErrorOr[Program] = {

    val dummyObj: Object = Object(
      name = ObjectName(None, "THIS NAME DOESN'T MATTER"),
      parent = None,
      nestedObjs = List(),
      callGraph = Map()
    )

    Traverse[Vector]
      .sequence(
        trees
          .map(restoreObjectFromTree(Tree((dummyObj, None), trees.toList)))
      )
      .map(objs => Program(objs.toList))
  }

  def findErrors(prog: Program): List[OdinAnalysisError] =
    prog.findCycles.map(cc => OdinAnalysisError(cc.show))

  def analyzeAst(prog: EOProg[EOExprOnly]): ErrorOr[List[OdinAnalysisError]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield findErrors(program)

  def main(args: Array[String]): Unit = {
    val code = exampleEO
    (for {
      ast <- sourceCodeEoParser[IO]().parse(code)
      _ <- advancedMutualRecursionAnalyzer[IO]
        .analyze(ast)
        .evalMap(IO.println)
        .compile
        .drain
    } yield ()).unsafeRunSync()

  }

  // TODO: this should be explicit
  implicit def advancedMutualRecursionAnalyzer[F[_]: Sync]: ASTAnalyzer[F] =
    new ASTAnalyzer[F] {

      override def analyze(
        ast: EOProg[EOExprOnly]
      ): Stream[F, OdinAnalysisError] = for {
        errors <- Stream.eval(
          Sync[F].fromEither(analyzeAst(ast).leftMap(new Exception(_)))
        )
        error <- Stream.emits(errors)
      } yield error

    }

}
