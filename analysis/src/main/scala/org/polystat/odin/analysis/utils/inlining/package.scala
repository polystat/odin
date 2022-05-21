package org.polystat.odin.analysis.utils.inlining

import cats.Applicative
import cats.data.NonEmptyVector
import cats.syntax.align._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.functor._
import cats.syntax.traverse._
import monocle.Optional
import org.polystat.odin.analysis.ObjectName
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.core.ast.EOBnd
import org.polystat.odin.core.ast.EOBndExpr
import org.polystat.odin.core.ast.EONamedBnd
import org.polystat.odin.core.ast.EOObj
import org.polystat.odin.core.ast.astparams.EOExprOnly

import types._

sealed trait BndPlaceholder
final case class MethodPlaceholder(name: EONamedBnd) extends BndPlaceholder
final case class ObjectPlaceholder(name: EONamedBnd) extends BndPlaceholder
final case class BndItself(bnd: EOBndExpr[EOExprOnly]) extends BndPlaceholder
final case class ParentPlaceholder(name: EOExprOnly) extends BndPlaceholder

final case class ObjectNameWithLocator(locator: BigInt, name: ObjectName)

sealed trait TraverseMethods[O[_, _]] {

  def traverseMethodInfo[F[_], P, M, MM](opm: O[P, M])(
    f: (EONamedBnd, M, O[P, M]) => F[MM]
  )(implicit F: Applicative[F]): F[O[P, MM]]

}

final case class ObjectTree[A](
  info: A,
  children: Map[EONamedBnd, ObjectTree[A]]
) {

  def zip[B, P, M](other: ObjectTree[B])(implicit
    ev: A <:< ObjectInfoForAnalysis[P, M]
  ): ObjectTree[(A, B)] = {
    ObjectTree(
      info = (info, other.info),
      children = children.alignWith(other.children)(_.onlyBoth.get match {
        case (this_, that) => this_.zip(that)
      })
    )
  }

  def traverseMethods[P, M, F[_], MM, O[_, _]](
    f: (EONamedBnd, M, O[P, M]) => F[MM]
  )(implicit
    T: TraverseMethods[O],
    F: Applicative[F],
    ev: A =:= O[P, M]
  ): F[ObjectTree[O[P, MM]]] =
    (
      T.traverseMethodInfo(info)(f),
      children
        .toList
        .traverse { case (k, o) =>
          o.traverseMethods(f).map((k, _))
        }
        .map(_.toMap)
    ).mapN((node, nestedObjects) => ObjectTree(node, nestedObjects))

}

final case class ObjectInfo[P, M](
  name: EONamedBnd,
  fqn: ObjectName,
  bnds: Vector[BndPlaceholder],
  depth: BigInt,
  parentInfo: Option[P],
  methods: Map[EONamedBnd, M],
)

object ObjectInfo {

  implicit val traverseMethodsObjectInfo: TraverseMethods[ObjectInfo] =
    new TraverseMethods[ObjectInfo] {

      override def traverseMethodInfo[F[_], P, M, MM](
        opm: ObjectInfo[P, M]
      )(
        f: (EONamedBnd, M, ObjectInfo[P, M]) => F[MM]
      )(implicit F: Applicative[F]): F[ObjectInfo[P, MM]] = {
        opm
          .methods
          .toList
          .traverse { case (name, m) =>
            f(name, m, opm).map(newM => (name, newM))
          }
          .map(m => opm.copy(methods = m.toMap))
      }

    }

}

final case class MethodInfoForAnalysis(
  body: EOObj[EOExprOnly],
  depth: BigInt
)

final case class ObjectInfoForAnalysis[P, M](
  methods: Map[EONamedBnd, M],
  parentInfo: Option[P],
  name: EONamedBnd,
  indirectMethods: Map[EONamedBnd, MethodInfoForAnalysis],
  allMethods: Map[EONamedBnd, MethodInfoForAnalysis]
)

object ObjectInfoForAnalysis {

  implicit val traverseMethodsForAnalysis: TraverseMethods[ObjectInfoForAnalysis] =
    new TraverseMethods[ObjectInfoForAnalysis] {

      override def traverseMethodInfo[F[_], P, M, MM](
        opm: ObjectInfoForAnalysis[P, M]
      )(
        f: (EONamedBnd, M, ObjectInfoForAnalysis[P, M]) => F[MM]
      )(implicit F: Applicative[F]): F[ObjectInfoForAnalysis[P, MM]] = opm
        .methods
        .toList
        .traverse { case (name, m) =>
          f(name, m, opm).map(newM => (name, newM))
        }
        .map(m => opm.copy(methods = m.toMap))

    }

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

final case class ParentInfo[M, O[_, _]](
  linkToParent: Optional[
    Map[EONamedBnd, ObjectTree[O[ParentInfo[M, O], M]]],
    ObjectTree[O[ParentInfo[M, O], M]]
  ]
) extends GenericParentInfo

final case class ParentInfoForInlining[M](
  parentMethods: Map[EONamedBnd, M]
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
       |${body.toEOPretty}
       |  bodyAfterInlining =
       |${bodyAfterInlining
        .expr
        .toEOPretty}
       |)
       |""".stripMargin

}

final case class Call(
  depth: BigInt,
  methodName: String,
  callSite: PathToCallSite,
  callLocation: PathToCall,
  args: NonEmptyVector[EOBnd[EOExprOnly]]
)
