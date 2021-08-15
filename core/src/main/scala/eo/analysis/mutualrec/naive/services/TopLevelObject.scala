package eo.analysis.mutualrec.naive.services

import cats.data.OptionT
import cats.effect.Sync
import cats.implicits._
import eo.analysis.mutualrec.naive.errors.DuplicatedMethodAttributes
import eo.core.ast.astparams.EOExprOnly
import eo.core.ast.{ EOBndExpr, EOObj }
import higherkindness.droste.data.Fix
import MethodAttribute.createMethodAttribute

trait TopLevelObject[F[_]] {
  def objName: String
  def attributes: F[Vector[MethodAttribute[F]]]
  def addMethodAttribute(expr: EOBndExpr[EOExprOnly]): F[Unit]
  def findAttributeWithName(name: String): OptionT[F, MethodAttribute[F]]
}

object TopLevelObject {
  def createTopLevelObjectWithRefs[
    F[_]: Sync
  ](
    objectName: String
  ): F[TopLevelObject[F]] = for {
    attrsMap <- Sync[F].delay(
      scala.collection.mutable.Map[
        String,
        MethodAttribute[F]
      ]()
    )
  } yield new TopLevelObject[F] {
    override def objName: String = objectName

    override def attributes: F[Vector[MethodAttribute[F]]] =
      Sync[F].delay {
        attrsMap.values.toVector
      }

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
    ): OptionT[F, MethodAttribute[F]] =
      OptionT(Sync[F].delay(attrsMap.get(name)))
  }
}
