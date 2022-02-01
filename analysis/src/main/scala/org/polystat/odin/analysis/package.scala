package org.polystat.odin

import cats.syntax.functor._
import cats.syntax.apply._
import cats.syntax.traverse._
import cats.Applicative
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.data._
import org.polystat.odin.analysis.data.Program._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._

package object analysis {

  type Method = (
    String, // method name
    EOObj[EOExprOnly] // method body
  )

  type PartialCall = (Option[ObjectName], String)
  type PartialCallGraphEntry = (MethodName, Set[PartialCall])
  type PartialCallGraph = Map[MethodName, Set[PartialCall]]

  type NestedObject = (
    String, // object name
    EOObj[EOExprOnly] // object body
  )

  def extractCalls[F[_]: Applicative](
    body: Vector[EOExpr[EOExprOnly]]
  )(
    getContainer: String => Option[ObjectName]
  ): F[Set[PartialCall]] =
    body.foldLeft[F[Set[PartialCall]]](Applicative[F].pure(Set.empty)) {
      case (acc, next) =>
        next match {
          case EOCopy(
                 Fix(EODot(Fix(EOSimpleApp("self")), name)),
                 args
               ) =>
            val accWithMethod =
              args
                .headOption
                .map(bnd => Fix.un(bnd.expr))
                .fold(acc) {
                  case EOSimpleApp("self") =>
                    acc.map(_ + ((getContainer(name), name)))
                  case _ => acc
                }

            val callsFromArgs =
              extractCalls(
                args.tail.map(bnd => Fix.un(bnd.expr))
              )(getContainer)

            (accWithMethod, callsFromArgs).mapN(_ union _)

          case EOCopy(Fix(trg), args) =>
            (
              acc,
              extractCalls(Vector(trg))(getContainer),
              extractCalls(
                args.value.map(bnd => Fix.un(bnd.expr))
              )(getContainer)
            ).mapN(_ union _ union _)
          case EODot(Fix(trg), _) =>
            (acc, extractCalls(Vector(trg))(getContainer)).mapN(_ union _)
          case _ => acc
        }
    }

  def extractCallGraph[F[_]: Applicative](
    container: ObjectName
  )(methods: Vector[Method]): F[PartialCallGraph] = {
    def extractCallGraphEntry(
      method: Method
    ): F[PartialCallGraphEntry] = {
      val (methodName, methodBody) = method
      extractCalls(
        methodBody.bndAttrs.map(bnd => Fix.un(bnd.expr))
      )(name => methods.find(_._1 == name).map(_ => container))
        .map(
          (MethodName(container, methodName), _)
        )
    }

    methods.traverse(extractCallGraphEntry).map(_.toMap)
  }

  def eoDotToObjectName(eoDot: EODot[EOExprOnly]): Option[ObjectName] =
    eoDot match {
      case EODot(EOSimpleApp(obj), attr) =>
        Some(ObjectName(Some(ObjectName(None, obj)), attr))
      case EODot(Fix(dot: EODot[EOExprOnly]), name) => eoDotToObjectName(dot)
          .map(container => ObjectName(Some(container), name))
      case _ => None
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

  def buildTree[F[_]: Applicative](
    prog: EOProg[EOExprOnly]
  ): F[Vector[Tree[PartialObject]]] =
    splitObjectBody(prog.bnds)
      .nestedObjects
      .traverse(buildTreeFromObj[F](None))

  def buildTreeFromObj[F[_]: Applicative](
    container: Option[ObjectName]
  )(
    obj: NestedObject
  ): F[Tree[PartialObject]] = {

    val (name, body) = obj
    val bodyInfo = splitObjectBody(body.bndAttrs)
    val objectName = ObjectName(container, name)

    (
      extractCallGraph(objectName)(bodyInfo.methods),
      bodyInfo.nestedObjects.traverse(buildTreeFromObj[F](Some(objectName)))
    ).mapN { (cg, nestedObjs) =>
      Tree(
        node =
          PartialObject(
            name = objectName,
            parentName = bodyInfo.parent,
            cg = cg
          ),
        children = nestedObjs.toList
      )
    }
  }

}
