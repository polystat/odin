package org.polystat.odin.analysis.mutualrec.advanced

import cats.syntax.either._
import cats.Traverse
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.core.ast.EOProg
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._

object Analyzer {
  type ErrorOr[A] = Either[String, A]

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

  case class PartialObject(
    name: ObjectName,
    cg: PartialCallGraph,
    parentName: Option[ObjectName], // parent or decoratee
  )

  type PartialCall = (Option[ObjectName], String)
  type PartialCallGraphEntry = (MethodName, Set[PartialCall])
  type PartialCallGraph = Map[MethodName, Set[PartialCall]]

  def untilDefined[A, B](lst: List[A])(f: A => Option[B]): Option[B] =
    lst match {
      case Nil => None
      case head :: tail => f(head).orElse(untilDefined(tail)(f))
    }

  sealed case class Tree[A](node: A, children: List[Tree[A]]) {

    def find(predicate: A => Boolean): Option[Tree[A]] =
      if (predicate(node))
        Some(Tree(node, children))
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
               ) =>
            acc.copy(nestedObjects = acc.nestedObjects.appended((name, obj)))

          // method
          // [self params] > methodName
          //   ...
          case EOBndExpr(
                 EOAnyNameBnd(LazyName(name)),
                 Fix(
                   obj @ EOObj(_ @LazyName("self") +: _, _, _)
                 )
               ) => acc.copy(methods =
              acc.methods.appended((name, obj))
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
  )(getContainer: String => ErrorOr[ObjectName]): ErrorOr[Set[PartialCall]] =
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
  )(methods: Vector[Method]): ErrorOr[PartialCallGraph] = {
    def extractCallGraphEntry(method: Method): ErrorOr[PartialCallGraphEntry] = {
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
                s"\"${container.show}\" calls a method \"$name\" that's not defined!"
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

//  def resolveCallGraph(progTree: Tree[PartialObject])(
//    parentName:
//    pcg: PartialCallGraph,
//  ): ErrorOr[CallGraph] = {
// // (Some(ObjectName(...)), "aboba") -> MethodName(ObjectName(...), "aboba")
//    // (None, "aboba") ->
//    ???
//  }

  def resolveParent(
    progTree: Tree[PartialObject] // only for lookup
  )(parentName: ObjectName): ErrorOr[Object] = {
    progTree
      .find { case PartialObject(objName, _, _) =>
        objName == parentName
      }
      .toRight("aboba")
      .flatMap {
        case Tree(
               PartialObject(parentObjName, parentCg, maybeParentOfParent),
               nestedTrees
             ) =>
          for {
            nestedObjs <- Traverse[List].sequence(
              nestedTrees.map(restoreObjectFromTree(progTree))
            )
            cg <- resolveCallGraph(parentCg)

            parent <- maybeParentOfParent match {
              case Some(parentOfParent) =>
                for {
                  parentOfParent <- resolveParent(progTree)(parentOfParent)
                } yield parentOfParent
                  .extended(parentObjName, cg)
                  .copy(nestedObjs = nestedObjs)
              case None =>
                Right(
                  Object(
                    name = parentObjName,
                    parent = None,
                    nestedObjs = nestedObjs,
                    callGraph = cg
                  )
                )
            }
          } yield parent
      }
  }

  def resolveCallGraph(
    partialObj: PartialObject,
    parentObject: Option[Object]
  ): ErrorOr[CallGraph] = {
    def resolveCall(parentObject: Option[Object])(
      call: String
    ): ErrorOr[MethodName] = ???
    def resolveCalls(parentObject: Option[Object])(
      pair: PartialCallGraphEntry
    ): ErrorOr[(MethodName, Set[MethodName])] = {
      val (method, partialCalls) = pair
      parentObject match {
        case Some(parent) => Traverse[List]
            .sequence(partialCalls.map {
              case (Some(whereDefined), name) => ???
              case (None, name) => parent
                  .callGraph
                  .keySet
                  .find(_.name == name)
                  .fold(resolveCall(parent.parent)(name))(Right(_))
            }.toList)
            .map(resolvedCalls => (method, resolvedCalls.toSet))

        case None => Traverse[List]
            .sequence(partialCalls.toList.map {
              case (Some(whereDefined), name) =>
                Right(MethodName(whereDefined, name))
              case (None, name) => Left(
                  s"Method \"$name\" is called, but is not defined in the object!"
                )
            })
            .map(calls => (method, calls.toSet))
      }
    }
    Traverse[List]
      .sequence(partialObj.cg.toList.map(resolveCalls(parentObject)))
      .map(_.toMap)
  }

  def restoreObjectFromTree(
    progTree: Tree[PartialObject] // only for lookup
  )(tree: Tree[PartialObject]): ErrorOr[Object] = {
    val PartialObject(partialObjName, callGraph, maybeParent) = tree.node
    maybeParent match {
      case Some(parent) => for {
          parentObj <- resolveParent(progTree)(parent).leftMap(_ =>
            s"Parent object ${parent.show} of ${partialObjName.show} is specified, but not defined in the program!"
          )
          resolvedCallGraph <- ???
          nestedObjs <- Traverse[List]
            .sequence(tree.children.map(restoreObjectFromTree(progTree)))
        } yield parentObj.extended(partialObjName, resolvedCallGraph).copy(nestedObjs = nestedObjs)
      case None =>
        for {
          nestedObjs <- Traverse[List].sequence(
            tree.children.map(restoreObjectFromTree(progTree))
          )
          cg <- resolveCallGraph(progTree)(callGraph)
        } yield Object(
          name = partialObjName,
          parent = None,
          nestedObjs = nestedObjs,
          callGraph = cg
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
    prog.findMultiObjectCycles.map(cc => OdinAnalysisError(cc.show))

  def analyzeAst(prog: EOProg[EOExprOnly]): ErrorOr[List[OdinAnalysisError]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield findErrors(program)

}
