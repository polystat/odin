package org.polystat.odin.analysis

import cats.Applicative
import cats.syntax.apply._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.polystat.odin.analysis.inlining.types._
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast.{EOBndExpr, EONamedBnd, EOObj}

package inlining {

  sealed trait BndPlaceholder
  final case class MethodPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class ObjectPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class BndItself(bnd: EOBndExpr[EOExprOnly]) extends BndPlaceholder

  final case class ObjectName(parent: Option[ObjectName], name: String)
  final case class ObjectNameWithLocator(locator: BigInt, name: ObjectName)

  sealed trait GenericObjectInfo[
    P <: GenericParentInfo,
    M <: GenericMethodInfo,
    O[_ <: GenericParentInfo, _ <: GenericMethodInfo]
  ] {
// type O[A <: GenericParentInfo, B <: GenericMethodInfo] =
    // GenericObjectInfo[A, B]
    val parentInfo: Option[P]
    val methodInfo: Map[EONamedBnd, M]

    def mapParent[
      PP <: GenericParentInfo,
    ](
      f: P => PP
    )(implicit ev: O[PP, M] <:< GenericObjectInfo[PP, M, O]): O[PP, M]

    def replaceParent[
      PP <: GenericParentInfo,
    ](
      other: PP
    )(implicit ev: O[PP, M] <:< GenericObjectInfo[PP, M, O]): O[PP, M] =
      mapParent(_ => other)(ev)

    def traverseMethodInfo[
      F[_]: Applicative,
      MM <: GenericMethodInfo,
    ](f: (EONamedBnd, M, O[P, M]) => F[MM])(implicit
      ev: O[P, M] <:< GenericObjectInfo[P, M, O]
    ): F[O[P, MM]]

  }

  final case class ObjectTree[
    P <: GenericParentInfo,
    M <: GenericMethodInfo,
    O[_ <: GenericParentInfo, _ <: GenericMethodInfo],
  ](
    node: O[P, M],
    children: Map[EONamedBnd, ObjectTree[P, M, O]]
  )(implicit ev: O[P, M] <:< GenericObjectInfo[P, M, O]) {

    def traverseParents[
      F[_]: Applicative,
      PP <: GenericParentInfo,
    ](
      f: O[P, M] => F[PP]
    )(implicit
      ev1: O[PP, M] <:< GenericObjectInfo[PP, M, O]
    ): F[ObjectTree[PP, M, O]] = {
      (
        f(node),
        children
          .toList
          .traverse { case (k, v) =>
            v.traverseParents(f).map(newP => (k, newP))
          }
          .map(_.toMap)
      )
        .mapN((p, nestedPs) => ObjectTree(node.replaceParent(p)(ev1), nestedPs))
    }

    def traverseMethods[
      F[_]: Applicative,
      MM <: GenericMethodInfo
    ](
      f: (EONamedBnd, M, O[P, M]) => F[MM]
    )(implicit
      ev1: O[P, MM] <:< GenericObjectInfo[P, MM, O]
    ): F[ObjectTree[P, MM, O]] =
      (
        node.traverseMethodInfo(f),
        children
          .toList
          .traverse { case (k, o) =>
            o.traverseMethods[F, MM](f).map((k, _))
          }
          .map(_.toMap)
      ).mapN((node, nestedObjects) => ObjectTree(node, nestedObjects))

  }

  final case class ObjectInfo[
    P <: GenericParentInfo,
    M <: GenericMethodInfo,
  ](
    name: EONamedBnd,
    bnds: Vector[BndPlaceholder],
    depth: BigInt,
    override val parentInfo: Option[P],
    override val methodInfo: Map[EONamedBnd, M],
  ) extends GenericObjectInfo[P, M, ObjectInfo] {

    override def mapParent[PP <: GenericParentInfo](f: P => PP)(implicit
      ev: ObjectInfo[PP, M] <:< GenericObjectInfo[PP, M, ObjectInfo]
    ): ObjectInfo[PP, M] = copy(parentInfo = parentInfo.map(f))

    override def traverseMethodInfo[F[_]: Applicative, MM <: GenericMethodInfo](
      f: (EONamedBnd, M, ObjectInfo[P, M]) => F[MM]
    )(implicit
      ev: ObjectInfo[P, M] <:< GenericObjectInfo[P, M, ObjectInfo]
    ): F[ObjectInfo[P, MM]] =
      methodInfo
        .toList
        .traverse { case (name, m) =>
          f(name, m, this).map(newM => (name, newM))
        }
        .map(_.toMap)

  }

  sealed trait GenericParentInfo

  final case class ParentName(name: ObjectNameWithLocator)
    extends GenericParentInfo

  final case class ParentInfo[M <: GenericMethodInfo](
    name: ObjectNameWithLocator,
    parentInfo: Option[ParentInfo[M]],
    methods: Map[EONamedBnd, M]
  ) extends GenericParentInfo

  sealed trait GenericMethodInfo

  final case class MethodInfo(
    calls: Vector[Call],
    body: EOObj[EOExprOnly],
    depth: BigInt,
  ) extends GenericMethodInfo

  final case class MethodInfoAfterInlining(
    body: EOObj[EOExprOnly],
    bodyAfterInlining: EOBndExpr[EOExprOnly],
  ) extends GenericMethodInfo {

    override def toString: String =
      s"""MethodInfoAfterInlining(
         |  body = 
         |${body
        .toEO
        .map(_.map((" " * 4).concat(_)))
        .map(_.mkString(util.Properties.lineSeparator))
        .merge}
         |  bodyAfterInlining = 
         |${bodyAfterInlining
        .expr
        .toEO
        .map(_.map((" " * 4).concat(_)))
        .map(_.mkString(util.Properties.lineSeparator))
        .merge}
         |)
         |""".stripMargin

  }

  final case class Call(
    depth: BigInt,
    methodName: String,
    callSite: PathToCallSite,
    callLocation: PathToCall,
    args: CopyArgs
  )

}
