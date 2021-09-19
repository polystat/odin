package org.polystat.odin.analysis.mutualrec.naive.services

import cats.effect.Sync
import cats.implicits._
import org.polystat.odin.core.ast.EOBnd
import org.polystat.odin.core.ast.astparams.EOExprOnly

trait MethodAttribute[F[_]] {
  import MethodAttribute.MethodInfo

  def name: String
  def params: Vector[String]
  def parentObject: TopLevelObject[F]
  def getMethodInfo: F[MethodInfo[F]]
  def referenceMethod(method: MethodAttribute[F]): F[Unit]
}

object MethodAttribute {
  type MethodBody = Vector[EOBnd[EOExprOnly]]
  type MethodReferences[F[_]] = scala.collection.immutable.Set[MethodAttribute[F]]

  case class MethodInfo[F[_]](methodBody: MethodBody, references: MethodReferences[F])

  def createMethodAttribute[F[_]: Sync](
    methodName: String,
    methodParams: Vector[String],
    parent: TopLevelObject[F],
    methodBody: MethodBody
  ): F[MethodAttribute[F]] = for {
    referencedMethodSet <- Sync[F].delay(
      scala.collection.mutable.Set[MethodAttribute[F]]()
    )
  } yield new MethodAttribute[F] {
    override def name: String = methodName

    override def params: Vector[String] = methodParams

    override def parentObject: TopLevelObject[F] = parent

    override def getMethodInfo: F[MethodInfo[F]] =
      Sync[F].delay(MethodInfo(
        methodBody,
        referencedMethodSet.toSet,
      ))

    override def referenceMethod(
      method: MethodAttribute[F]
    ): F[Unit] = Sync[F].delay {
      referencedMethodSet.add(method)
      ()
    }
  }
}
