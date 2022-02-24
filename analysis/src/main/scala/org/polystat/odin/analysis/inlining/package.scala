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

  final case class Object[M <: GenericMethodInfo](
    name: EONamedBnd,
    methods: Map[EONamedBnd, M],
    nestedObjects: Map[EONamedBnd, Object[M]],
    bnds: Vector[BndPlaceholder],
    depth: BigInt,
  ) {

    def traverse[F[_]: Applicative, B <: GenericMethodInfo](
      f: (EONamedBnd, M, Object[M]) => F[B]
    ): F[Object[B]] =
      (
        methods
          .toList
          .traverse { case (k, v) => f(k, v, this).map((k, _)) }
          .map(_.toMap),
        nestedObjects
          .toList
          .traverse { case (k, o) => o.traverse(f).map((k, _)) }
          .map(_.toMap)
      ).mapN((methods, nestedObjects) =>
        copy(
          methods = methods,
          nestedObjects = nestedObjects
        )
      )

  }

//  case class Assert()

  sealed trait GenericMethodInfo

  sealed trait Calls extends GenericMethodInfo {
    val calls: Vector[Call]
  }

  sealed trait Body extends GenericMethodInfo {
    val body: EOObj[EOExprOnly]
  }

  sealed trait Depth extends GenericMethodInfo {
    val depth: BigInt
  }

  sealed trait BodyAfterInlining extends GenericMethodInfo {
    val bodyAfterInlining: EOBndExpr[EOExprOnly]
  }

  final case class MethodInfo(
    override val calls: Vector[Call],
    override val body: EOObj[EOExprOnly],
    override val depth: BigInt,
  ) extends Body
       with Calls
       with Depth

  final case class MethodInfoAfterInlining(
    override val body: EOObj[EOExprOnly],
    override val bodyAfterInlining: EOBndExpr[EOExprOnly],
  ) extends Body
       with BodyAfterInlining {

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
