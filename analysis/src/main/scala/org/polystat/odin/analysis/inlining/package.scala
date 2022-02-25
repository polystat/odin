package org.polystat.odin.analysis

import org.polystat.odin.core.ast.{EOBndExpr, EONamedBnd, EOObj}
import org.polystat.odin.core.ast.astparams.EOExprOnly
import inlining.types._

import cats.Applicative
import cats.syntax.traverse._
import cats.syntax.functor._
import cats.syntax.apply._

import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.ToEO.instances._

package inlining {

  sealed trait BndPlaceholder
  final case class MethodPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class ObjectPlaceholder(name: EONamedBnd) extends BndPlaceholder
  final case class BndItself(bnd: EOBndExpr[EOExprOnly]) extends BndPlaceholder

  final case class ObjectName(parent: Option[ObjectName], name: String)
  final case class ObjectNameWithLocator(locator: BigInt, name: ObjectName)

  final case class Object[
    M <: GenericMethodInfo,
    P <: GenericParentInfo,
  ](
    name: EONamedBnd,
    parentInfo: Option[P],
    methods: Map[EONamedBnd, M],
    nestedObjects: Map[EONamedBnd, Object[M, P]],
    bnds: Vector[BndPlaceholder],
    depth: BigInt,
  ) {

    def traverseParents[F[_]: Applicative, PP <: GenericParentInfo](
      f: Object[M, P] => F[PP]
    ): F[Object[M, PP]] = {
      (
        f(this),
        nestedObjects
          .toList
          .traverse { case (k, v) =>
            v.traverseParents(f).map(newP => (k, newP))
          }
          .map(_.toMap)
      )
        .mapN((p, nestedPs) =>
          copy(
            parentInfo = this.parentInfo.map(_ => p),
            nestedObjects = nestedPs
          )
        )
    }

    def traverseMethods[F[_]: Applicative, MM <: GenericMethodInfo](
      f: (EONamedBnd, M, Object[M, P]) => F[MM]
    ): F[Object[MM, P]] =
      (
        methods
          .toList
          .traverse { case (k, v) => f(k, v, this).map((k, _)) }
          .map(_.toMap),
        nestedObjects
          .toList
          .traverse { case (k, o) => o.traverseMethods(f).map((k, _)) }
          .map(_.toMap)
      ).mapN((methods, nestedObjects) =>
        copy(
          methods = methods,
          nestedObjects = nestedObjects
        )
      )

  }

  sealed trait GenericParentInfo

  final case class ParentName(name: ObjectNameWithLocator)
    extends GenericParentInfo

  final case class ParentInfo[M <: GenericMethodInfo](
    name: ObjectNameWithLocator,
    parentInfo: Option[ParentInfo[M]],
    methods: Map[EONamedBnd, M]
  )

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
