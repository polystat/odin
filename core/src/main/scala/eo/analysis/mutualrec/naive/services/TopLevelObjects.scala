package eo.analysis.mutualrec.naive.services

import cats.effect.Sync
import cats.implicits._
import eo.analysis.mutualrec.naive.services.TopLevelObject.createTopLevelObjectWithRefs
import eo.core.ast.EOObj
import eo.core.ast.astparams.EOExprOnly

trait TopLevelObjects[F[_]] {
  def objects: F[Vector[TopLevelObject[F]]]
  def add(objName: String, obj: EOObj[EOExprOnly]): F[Unit]
  def findMethodsWithParamsByName(methodName: String): F[Vector[MethodAttribute[F]]]
}

object TopLevelObjects {
  def createTopLevelObjectsWithRefs[
    F[_]: Sync
  ]: F[TopLevelObjects[F]] =
    for {
      objsMap <- Sync[F].delay(
        scala.collection.mutable.Map[
          String,
          TopLevelObject[F]
        ]()
      )
    } yield new TopLevelObjects[F] {
      override def objects: F[Vector[TopLevelObject[F]]] =
        Sync[F].delay {
          objsMap.values.toVector
        }

      override def add(
        objName: String, obj: EOObj[EOExprOnly]
      ): F[Unit] = for {
        methodAttrs <- createTopLevelObjectWithRefs[F](objName)
        _ <- obj.bndAttrs.traverse_ { objBodyAttr =>
          methodAttrs.addMethodAttribute(objBodyAttr)
        }
        _ <- Sync[F].delay {
          objsMap += (objName -> methodAttrs)
        }
      } yield ()

      override def findMethodsWithParamsByName(
        methodName: String
      ): F[Vector[MethodAttribute[F]]] = for {
        objects <- Sync[F].delay(objsMap.toVector.map(_._2))
        methods <- objects.flatTraverse(_.attributes)
        result = methods
          .filter(_.name == methodName)
          .filter(_.params.nonEmpty)
      } yield result
    }
}
