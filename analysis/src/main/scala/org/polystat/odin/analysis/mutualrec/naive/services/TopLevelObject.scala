package org.polystat.odin.analysis.mutualrec.naive.services

import cats.data.OptionT
import cats.effect.Sync
import cats.effect.Ref
import cats.implicits._
import org.polystat.odin.analysis.mutualrec.naive.exceptions.DuplicatedMethodAttributes
import MethodAttribute.createMethodAttribute
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast.{EOBndExpr, EOObj}

trait TopLevelObject[F[_]] {
  def objName: String
  def attributes: F[Vector[MethodAttribute[F]]]
  def baseObject: OptionT[F, TopLevelObject[F]]
  def inherit(topLevelObject: TopLevelObject[F]): F[Unit]
  def addMethodAttribute(expr: EOBndExpr[EOExprOnly]): F[Unit]
  def findAttributeWithName(name: String): OptionT[F, MethodAttribute[F]]
}

object TopLevelObject {

  def createTopLevelObject[F[_]: Sync](
    objectName: String
  ): F[TopLevelObject[F]] =
    for {
      attrsMap <- Sync[F].delay(
        scala
          .collection
          .mutable
          .Map[
            String,
            MethodAttribute[F]
          ]()
      )
      baseObjState <- Ref.of[F, Option[TopLevelObject[F]]](None)
    } yield new TopLevelObject[F] {
      override def objName: String = objectName

      override def attributes: F[Vector[MethodAttribute[F]]] =
        Sync[F].delay {
          attrsMap.values.toVector
        }

      override def baseObject: OptionT[F, TopLevelObject[F]] =
        OptionT(baseObjState.get)

      override def inherit(topLevelObject: TopLevelObject[F]): F[Unit] =
        baseObjState.set(Some(topLevelObject))

      override def addMethodAttribute(
        expr: EOBndExpr[EOExprOnly]
      ): F[Unit] = {
        val methodName = expr.bndName.name.name

        if (attrsMap.contains(methodName))
          Sync[F].raiseError(DuplicatedMethodAttributes(objectName, methodName))
        else {
          Fix.un(expr.expr) match {
            case EOObj(freeAttrs, varargAttr, methBodyAttrs) =>
              for {
                methodAttr <- createMethodAttribute(
                  methodName,
                  freeAttrs.map(_.name) ++ varargAttr.map(_.name),
                  this,
                  methBodyAttrs,
                )
                _ <- Sync[F].delay {
                  attrsMap += (methodName -> methodAttr)
                  ()
                }
              } yield ()
            // Do not take in consideration other attributes, since they are
            // not considered as "method-attributes", which are of pattern:
            //   [self param1] > methodAttr
            //     ...
            case _ => Sync[F].pure(())
          }
        }
      }

      override def findAttributeWithName(
        name: String
      ): OptionT[F, MethodAttribute[F]] = {
        val currentObjectMethod = OptionT.fromOption[F](attrsMap.get(name))
        val inheritedObjectMethod =
          baseObject.flatMap(_.findAttributeWithName(name))
        currentObjectMethod.orElse(inheritedObjectMethod)
      }

    }

}
