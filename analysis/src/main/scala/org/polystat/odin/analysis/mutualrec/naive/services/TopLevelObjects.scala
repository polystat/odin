package org.polystat.odin.analysis.mutualrec.naive.services

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.mutualrec.naive.exceptions.DecorateeNotFound
import org.polystat.odin.analysis.mutualrec.naive.exceptions.UnsupportedDecoration
import org.polystat.odin.core.ast.EOApp
import org.polystat.odin.core.ast.EOBndExpr
import org.polystat.odin.core.ast.EOCopy
import org.polystat.odin.core.ast.EODecoration
import org.polystat.odin.core.ast.EOObj
import org.polystat.odin.core.ast.EOSimpleApp
import org.polystat.odin.core.ast.astparams.EOExprOnly

import scala.annotation.tailrec

import TopLevelObject.createTopLevelObject

trait TopLevelObjects[F[_]] {
  def objects: F[Vector[TopLevelObject[F]]]
  def add(objName: String, obj: EOObj[EOExprOnly]): F[Unit]
  def findObjectByName(objectName: String): OptionT[F, TopLevelObject[F]]

  def findMethodsWithParamsByName(
    methodName: String
  ): F[Vector[MethodAttribute[F]]]

}

object TopLevelObjects {

  def createTopLevelObjectsWithRefs[F[_]: Sync]: F[TopLevelObjects[F]] =
    for {
      objsMap <- Sync[F].delay(
        scala
          .collection
          .mutable
          .Map[
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
        objName: String,
        obj: EOObj[EOExprOnly]
      ): F[Unit] = for {
        topLevelObject <- createTopLevelObject[F](objName)
        _ <- obj.bndAttrs.traverse_ {
          case EOBndExpr(EODecoration, Fix(decorateeExpr: EOApp[EOExprOnly])) =>
            // For simplicity right now consider only trivial object decoration
            for {
              decorateeName <- findDecorateeName(decorateeExpr)
                .map(Sync[F].delay(_))
                .getOrElse(Sync[F].raiseError(UnsupportedDecoration(objName)))
              maybeDecoratedObject <- findObjectByName(decorateeName).value
              decorateeObject <- maybeDecoratedObject
                .map(Sync[F].delay(_))
                .getOrElse(
                  Sync[F].raiseError(DecorateeNotFound(objName, decorateeName))
                )
              _ <- topLevelObject.inherit(decorateeObject)
            } yield ()
          case objBodyAttr => topLevelObject.addMethodAttribute(objBodyAttr)
        }
        _ <- Sync[F].delay {
          objsMap += (objName -> topLevelObject)
        }
      } yield ()

      override def findObjectByName(
        objectName: String
      ): OptionT[F, TopLevelObject[F]] = OptionT(Sync[F].delay {
        objsMap.get(objectName)
      })

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

  @tailrec
  private def findDecorateeName(
    decorateeExpr: EOApp[EOExprOnly]
  ): Option[String] = decorateeExpr match {
    case EOSimpleApp(name) => Some(name)
    case EOCopy(Fix(copyTarget: EOApp[EOExprOnly]), _) =>
      findDecorateeName(copyTarget)
    case _ => None
  }

}
