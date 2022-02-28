package org.polystat.odin.analysis

import cats.Applicative
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import org.polystat.odin.analysis.inlining.types._
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast.{EOBndExpr, EONamedBnd, EOObj}
import cats.data.{NonEmptyList => Nel}

package inlining {

  import monocle.Optional

  sealed trait BndPlaceholder
  final case class MethodPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class ObjectPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class BndItself(bnd: EOBndExpr[EOExprOnly]) extends BndPlaceholder

  final case class ObjectName(names: Nel[String])
  final case class ObjectNameWithLocator(locator: BigInt, name: ObjectName)

  sealed trait GenericObjectInfo[
    P <: GenericParentInfo,
    M <: GenericMethodInfo,
    O[
      _ <: GenericParentInfo,
      _ <: GenericMethodInfo
    ] <: GenericObjectInfo[_, _, O]
  ] {

    val parentInfo: Option[P]
    val methods: Map[EONamedBnd, M]

    def modifyParent[
      PP <: GenericParentInfo,
    ](
      f: P => Option[PP]
    ): O[PP, M]

    def replaceParent[
      PP <: GenericParentInfo,
    ](
      other: Option[PP]
    ): O[PP, M] =
      modifyParent(_ => other)

    def traverseMethodInfo[
      F[_]: Applicative,
      MM <: GenericMethodInfo,
    ](f: (EONamedBnd, M, O[P, M]) => F[MM]): F[O[P, MM]]

  }

  final case class ObjectTree[
    P <: GenericParentInfo,
    M <: GenericMethodInfo,
    O[
      _ <: GenericParentInfo,
      _ <: GenericMethodInfo
    ] <: GenericObjectInfo[_, _, O],
  ](
    info: O[P, M],
    children: Map[EONamedBnd, ObjectTree[P, M, O]]
  ) {

    def traverseParents[
      F[_]: Applicative,
      PP <: GenericParentInfo,
    ](
      f: O[P, M] => F[Option[PP]]
    )(implicit
      ev: O[P, M] <:< GenericObjectInfo[P, M, O]
    ): F[ObjectTree[PP, M, O]] = {
      (
        f(info),
        children
          .toList
          .traverse { case (k, v) =>
            v.traverseParents(f).map(newP => (k, newP))
          }
          .map(_.toMap)
      )
        .mapN((p, nestedPs) => ObjectTree(ev(info).replaceParent(p), nestedPs))
    }

    def traverseMethods[
      F[_]: Applicative,
      MM <: GenericMethodInfo
    ](
      f: (EONamedBnd, M, O[P, M]) => F[MM]
    )(implicit
      ev: O[P, M] <:< GenericObjectInfo[P, M, O]
    ): F[ObjectTree[P, MM, O]] =
      (
        ev(info).traverseMethodInfo(f),
        children
          .toList
          .traverse { case (k, o) =>
            o.traverseMethods(f).map((k, _))
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
    override val methods: Map[EONamedBnd, M],
  ) extends GenericObjectInfo[P, M, ObjectInfo] {

    override def modifyParent[PP <: GenericParentInfo](
      f: P => Option[PP]
    ): ObjectInfo[PP, M] = copy(parentInfo = parentInfo.flatMap(f))

    override def traverseMethodInfo[F[_]: Applicative, MM <: GenericMethodInfo](
      f: (EONamedBnd, M, ObjectInfo[P, M]) => F[MM]
    ): F[ObjectInfo[P, MM]] =
      methods
        .toList
        .traverse { case (name, m) =>
          f(name, m, this).map(newM => (name, newM))
        }
        .map(m => copy(methods = m.toMap))

  }

  sealed trait GenericParentInfo

  final case class ParentName(name: ObjectNameWithLocator)
    extends GenericParentInfo {

    def toEOName: String = {
      val locator =
        if (name.locator == 0) "$"
        else List.fill(name.locator.toInt)("^").mkString(".")
      val names = name.name.names.mkString_(".")
      s"$locator.$names"
    }

  }

  final case class ParentInfo[
    M <: GenericMethodInfo,
    O[
      _ <: GenericParentInfo,
      _ <: GenericMethodInfo
    ] <: GenericObjectInfo[_, _, O],
  ](
    linkToParent: Optional[
      Map[EONamedBnd, ObjectTree[ParentInfo[M, O], M, O]],
      ObjectTree[ParentInfo[M, O], M, O]
    ]
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
