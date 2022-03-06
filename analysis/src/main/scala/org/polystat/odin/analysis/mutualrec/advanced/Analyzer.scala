package org.polystat.odin.analysis.mutualrec.advanced

import cats.MonadError
import cats.data.{NonEmptyList => Nel}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.EOOdinAnalyzer.OdinAnalysisError
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._
import org.polystat.odin.analysis.mutualrec.advanced.Program._
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

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

//  def buildObjectTree(
//    prog: EOProg[EOExprOnly]
//  ) = {
//    for {
//      partialTree <- Inliner.createObjectTree(prog)
//      treeWithParents <- Inliner.resolveParents(partialTree)
//      program <- buildProgramFromObjectTree(treeWithParents)
//    } yield program
//  }
//
//  def buildProgramFromObjectTree(
//    objTree: Map[EONamedBnd, Inliner.CompleteObjectTree]
//  ): EitherNel[String, Program] = {
//
//    @unused def recurseCurrentLevel(
//      currentLevel: Map[EONamedBnd, Inliner.CompleteObjectTree]
//    )(curNames: Nel[String]): EitherNel[String, Program] = {
//      currentLevel.toList.traverse { case (_, tree) =>
//        recurse(tree)(curNames)
//      }
//    }
//
//
//    def extractCallGraph(
//      tree: Inliner.CompleteObjectTree
//    )(methods: Map[EONamedBnd, inlining.MethodInfo]): CallGraph = {
//      val objectName = tree.info.fqn
//      methods.map { case (_, _) =>
//        val methodName = MethodName(objectName, name.name.name)
//        val calls = info.calls
//        ???
//      }
//
//    }
//
//    def recurse(
//      curTree: Inliner.CompleteObjectTree
//    )(curNames: Nel[String]): EitherNel[String, Object] = {
//      val objectName = curNames.concatNel(Nel.one(curTree.info.name.name.name))
//      val parentInfo = ???
//      val nestedObjs = recurseCurrentLevel(curTree.children)(objectName)
//      val callGraph = extractCallGraph(objectName)(curTree.info.methods)
//      ???
//    }
//
//    objTree.toList.traverse { case (name, tree) =>
//      recurse(tree)(Nel.one(name.name.name))
//    }
//  }

  type PartialCall = (Option[ObjectName], String)
  type PartialCallGraphEntry = (MethodName, Set[PartialCall])
  type PartialCallGraph = Map[MethodName, Set[PartialCall]]

  final case class Tree[A](node: A, children: List[Tree[A]]) {

    private def untilDefined[B, C](lst: List[B])(f: B => Option[C]): Option[C] =
      lst match {
        case Nil => None
        case head :: tail => f(head).orElse(untilDefined(tail)(f))
      }

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
               ) => acc.copy(parent = Some(ObjectName(name)))

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
        Some(ObjectName(Nel(obj, List(attr))))
      case EODot(Fix(dot: EODot[EOExprOnly]), name) => eoDotToObjectName(dot)
          .map(container => ObjectName(container.names.append(name)))
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
    methods.traverse(extractCallGraphEntry).map(_.toMap)
  }

  def buildTreeFromObj[F[_]: MonadError[*[_], String]](
    container: Option[ObjectName]
  )(
    obj: NestedObject
  ): F[Tree[PartialObject]] = {

    val (name, body) = obj
    val bodyInfo = splitObjectBody(body.bndAttrs)
    val objectName = ObjectName.fromContainer(container, name)

    for {
      cg <- extractCallGraph(objectName)(bodyInfo.methods)
      nestedObjs <-
        bodyInfo.nestedObjects.traverse(buildTreeFromObj[F](Some(objectName)))
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
    splitObjectBody(prog.bnds)
      .nestedObjects
      .traverse(buildTreeFromObj[F](None))

  def resolveParent[F[_]](
    progTree: Tree[PartialObject] // only for lookup
  )(
    of: ObjectName, /* object whose parent is being resolved, used for error
     * reporting */
    maybeParentName: Option[ObjectName], // parent object name
  )(implicit F: MonadError[F, String]): F[Option[ParentInfo]] = {
    // returns:
    // F.pure(None) - parent was not found and should not be found
    // F.raiseError(...) - some exceptional case
    // F.pure(Some(...)) - parent was found
    maybeParentName match {
      case Some(parentName) =>
        progTree
          .find(_.name == parentName)
          .fold[F[Option[ParentInfo]]](
            F.raiseError(
              s"Parent (or decoratee) object \"${parentName.show}\" of object \"${of.show}\" " +
                s"is specified, but not defined in the program!"
            )
          ) {
            case PartialObject(parentObjName, parentCg, maybeParentOfParent) =>
              for {
                parentOfParent <-
                  resolveParent(progTree)(parentObjName, maybeParentOfParent)
                cg <- convertPartialCg(parentObjName, parentCg, parentOfParent)
              } yield Some(
                ParentInfo(
                  name = parentObjName,
                  callGraph =
                    parentOfParent.fold(cg)(_.callGraph.extendWith(cg)),
                  parent = parentOfParent
                )
              )
          }
      case None => F.pure(None)
    }
  }

  def convertPartialCg[F[_]](
    objectName: ObjectName, /* where call graph is defined, used for error
     * reporting */
    pcg: PartialCallGraph, // call graph to resolve
    maybeParent: Option[ParentInfo] /* the parent object to resolve methods from
     * parent obj */
  )(implicit F: MonadError[F, String]): F[CallGraph] = {

    def createErrorMsg(methodName: String): String = {
      s"Method \"$methodName\" was called from the object \"${objectName.show}\"," +
        s" although it is not defined there!"
    }

    def resolveCallWithParent(
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
                  .fold[F[MethodName]](
                    resolveCallWithParent(parentOfParent)(pc)
                  )(F.pure)
              case None => F.raiseError(createErrorMsg(name))
            })(F.pure)
      }
    }

    def resolveCallNoParent(pc: PartialCall): F[MethodName] = pc match {
      case (Some(whereDefined), name) =>
        F.pure(MethodName(whereDefined, name))
      case (None, name) => F.raiseError(createErrorMsg(name))
    }

    val resolveCall: PartialCall => F[MethodName] = maybeParent match {
      case Some(parent) => resolveCallWithParent(parent)
      case None => resolveCallNoParent
    }

    pcg
      .toList
      .traverse { case (methodName, partialCalls) =>
        partialCalls
          .toList
          .traverse(resolveCall)
          .map(calls => (methodName, calls.toSet))
      }
      .map(_.toMap)
  }

  def restoreObjectFromTree[F[_]](
    progTree: Tree[PartialObject] // only for lookup
  )(tree: Tree[PartialObject])(implicit F: MonadError[F, String]): F[Object] = {
    val PartialObject(partialObjName, pcg, maybeParent) = tree.node
    for {
      parent <- resolveParent[F](progTree)(partialObjName, maybeParent)
      cg <- convertPartialCg[F](partialObjName, pcg, parent)
      nestedObjs <- tree.children.traverse(restoreObjectFromTree[F](progTree))
    } yield Object(
      name = partialObjName,
      parent = parent,
      callGraph = parent.fold(cg)(_.callGraph.extendWith(cg)),
      nestedObjs = nestedObjs
    )
  }

  def buildProgram[F[_]](
    trees: Vector[Tree[PartialObject]]
  )(implicit F: MonadError[F, String]): F[Program] = {

    val dummyObj: PartialObject = PartialObject(
      name = ObjectName("THIS NAME DOESN'T MATTER"),
      parentName = None,
      cg = Map(),
    )

    trees
      .traverse(restoreObjectFromTree[F](Tree(dummyObj, trees.toList)))
      .map(objs => objs.toList)
  }

  private[analysis] def produceChains[F[_]: MonadError[*[_], String]](
    prog: EOProg[EOExprOnly]
  ): F[List[CallChain]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield program.findMultiObjectCycles

  def filterCycleShifts(
    ccs: List[(ObjectName, CallChain)]
  ): List[(ObjectName, CallChain)] = {

    val ord = new Ordering[(ObjectName, CallChain)] {
      def compare(
        i: (ObjectName, CallChain),
        j: (ObjectName, CallChain)
      ): Int =
        if (i._2.isShiftOf(j._2)) 1 else 0
    }

    collection.immutable.SortedSet(ccs: _*)(ord).toList
  }

  def analyzeAst[F[_]](
    prog: EOProg[EOExprOnly]
  )(implicit F: MonadError[F, String]): F[List[OdinAnalysisError]] =
    for {
      tree <- buildTree(prog)
      program <- buildProgram(tree)
    } yield filterCycleShifts(program.findMultiObjectCyclesWithObject).map {
      case (objName, ccs) =>
        val fancyChain = ccs
          .map(methodName =>
            if (objName == methodName.whereDefined) methodName.show
            else methodName
              .copy(whereDefined = objName)
              .show + s" (was last redefined in \"${methodName.whereDefined.show}\")"
          )
          .mkString(" -> ")

        OdinAnalysisError(s"${objName.show}: $fancyChain")
    }

}
