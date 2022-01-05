package org.polystat.odin.analysis.mutualrec.advanced

import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.{ApplicativeError, MonadError, Traverse}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.core.ast.EOProg
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._

object Analyzer {

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

  def extractCalls[F[_]](
    body: Vector[EOExpr[EOExprOnly]]
  )(
    getContainer: String => Option[ObjectName]
  )(implicit F: MonadError[F, String]): F[Set[PartialCall]] =
    body.foldLeft[F[Set[PartialCall]]](F.pure(Set.empty)) { case (acc, next) =>
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
                  acc.map(_ + ((getContainer(name), name)))
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

  def extractCallGraph[F[_]: MonadError[*[_], String]](
    container: ObjectName
  )(methods: Vector[Method]): F[PartialCallGraph] = {
    def extractCallGraphEntry(
      method: Method
    ): F[PartialCallGraphEntry] = {
      val (methodName, methodBody) = method
      for {
        calls <- extractCalls(
          methodBody.bndAttrs.map(bnd => Fix.un(bnd.expr))
        ) { name => methods.find(_._1 == name).map(_ => container) }
      } yield (
        MethodName(container, methodName),
        calls
      )
    }
    Traverse[Vector].sequence(methods.map(extractCallGraphEntry)).map(_.toMap)
  }

  def buildTreeFromObj[F[_]: MonadError[*[_], String]](
    container: Option[ObjectName]
  )(
    obj: NestedObject
  ): F[Tree[PartialObject]] = {

    val (name, body) = obj
    val bodyInfo = splitObjectBody(body.bndAttrs)
    val objectName = ObjectName(container, name)

    for {
      cg <- extractCallGraph(objectName)(bodyInfo.methods)
      nestedObjs <- Traverse[Vector].sequence(
        bodyInfo.nestedObjects.map(buildTreeFromObj[F](Some(objectName)))
      )
    } yield Tree(
      node =
        PartialObject(
          name = objectName,
          parentName = bodyInfo.parent,
          cg = cg
        ),
      children = nestedObjs.toList
    )
  }

  def buildTree[F[_]: MonadError[*[_], String]](
    prog: EOProg[EOExprOnly]
  ): F[Vector[Tree[PartialObject]]] =
    Traverse[Vector].sequence(
      splitObjectBody(prog.bnds).nestedObjects.map(buildTreeFromObj[F](None))
    )

  def resolveParent[F[_]](
    progTree: Tree[PartialObject] // only for lookup
  )(
    parentName: ObjectName
  )(implicit F: MonadError[F, String]): F[ParentInfo] = {
    F.fromOption(
      progTree
        .find(_.name == parentName),
      "aboba"
    ).flatMap {
      case PartialObject(parentObjName, parentCg, maybeParentOfParent) =>
        for {
          parent <- maybeParentOfParent match {
            case Some(parentOfParent) =>
              for {
                parentOfParent <- resolveParent(progTree)(parentOfParent)
                cg <- convertPartialCgWithParent(parentCg, parentOfParent)
              } yield ParentInfo(
                name = parentObjName,
                callGraph = parentOfParent.callGraph.extendWith(cg),
                parent = Some(parentOfParent)
              )
            case None =>
              convertPartialCgNoParent(parentCg).map(cg =>
                ParentInfo(
                  name = parentObjName,
                  parent = None,
                  callGraph = cg,
                )
              )

          }
        } yield parent
    }
  }

  def convertPartialCgWithParent[F[_]](
    pcg: PartialCallGraph,
    parent: ParentInfo
  )(implicit F: ApplicativeError[F, String]): F[CallGraph] = {

    def convertCall(
      parent: ParentInfo
    )(pc: PartialCall): F[MethodName] = {
      pc match {
        case (Some(whereDefined), name) =>
          F.pure(MethodName(whereDefined, name))
        case (None, name) =>
          parent
            .callGraph
            .keySet
            .find(_.name == name)
            .fold[F[MethodName]](parent.parent match {
              case Some(parentOfParent) => parent
                  .callGraph
                  .keySet
                  .find(_.name == name)
                  .fold[F[MethodName]](convertCall(parentOfParent)(pc))(F.pure)
              case None =>
                F.raiseError(
                  s"Method \"$name\" was called, but it doesn't exist!"
                )
            })(F.pure)
      }
    }

    convertPartialCg(pcg)(convertCall(parent))
  }

  def convertPartialCg[F[_]](pcg: PartialCallGraph)(
    resolveCall: PartialCall => F[MethodName]
  )(implicit F: ApplicativeError[F, String]): F[CallGraph] =
    Traverse[List]
      .sequence(pcg.toList.map { case (methodName, partialCalls) =>
        Traverse[List]
          .sequence(partialCalls.toList.map(resolveCall))
          .map(calls => (methodName, calls.toSet))
      })
      .map(_.toMap)

  def convertPartialCgNoParent[F[_]](
    pcg: PartialCallGraph
  )(implicit F: ApplicativeError[F, String]): F[CallGraph] =
    convertPartialCg[F](pcg) {
      case (Some(whereDefined), name) =>
        F.pure(MethodName(whereDefined, name))
      case (None, name) => F.raiseError(
          s"Method \"$name\" is called, but is not defined in the object!"
        )
    }

  def restoreObjectFromTree[F[_]](
    progTree: Tree[PartialObject] // only for lookup
  )(tree: Tree[PartialObject])(implicit F: MonadError[F, String]): F[Object] = {
    val PartialObject(partialObjName, pcg, maybeParent) = tree.node
    maybeParent match {
      case Some(parent) =>
        for {
          parentInfo <- F.adaptError(resolveParent(progTree)(parent))(_ =>
            s"Parent object ${parent.show} of ${partialObjName.show} is specified, but not defined in the program!"
          )
          cg <- convertPartialCgWithParent(pcg, parentInfo)
          nestedObjs <- Traverse[List].sequence(
            tree.children.map(restoreObjectFromTree[F](progTree))
          )
        } yield Object(
          parent = Some(parentInfo),
          name = partialObjName,
          callGraph = parentInfo.callGraph.extendWith(cg),
          nestedObjs = nestedObjs,
        )
      case None =>
        for {
          nestedObjs <- Traverse[List].sequence(
            tree.children.map(restoreObjectFromTree[F](progTree))
          )
          cg <- convertPartialCgNoParent(pcg)
        } yield Object(
          name = partialObjName,
          parent = None,
          nestedObjs = nestedObjs,
          callGraph = cg
        )
    }
  }

  def buildProgram[F[_]](
    trees: Vector[Tree[PartialObject]]
  )(implicit F: MonadError[F, String]): F[Program] = {

    val dummyObj: PartialObject = PartialObject(
      name = ObjectName(None, "THIS NAME DOESN'T MATTER"),
      parentName = None,
      cg = Map(),
    )

    Traverse[Vector]
      .sequence(
        trees
          .map(restoreObjectFromTree[F](Tree(dummyObj, trees.toList)))
      )
      .map(objs => Program(objs.toList))
  }

  def findErrors(prog: Program): List[OdinAnalysisError] =
    prog.findMultiObjectCycles.map(cc => OdinAnalysisError(cc.show))

  def analyzeAst[F[_]](
    prog: EOProg[EOExprOnly]
  )(implicit F: MonadError[F, String]): F[List[OdinAnalysisError]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield findErrors(program)

}
