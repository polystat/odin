package eo.analysis.mutualrec.naive
import cats.data._
import eo.core.ast._
import eo.core.ast.astparams.EOExprOnly
import higherkindness.droste.data.Fix

object mutualrec {
  import eo.analysis.mutualrec.naive.mutualrec.effects.{ MethodAttribute, TopLevelObjects }

  type MethodAttributeRefStateRec[F[_], S] = (
    scala.collection.immutable.Set[MethodAttribute[F, S]],
    Vector[EOBnd[EOExprOnly]]
  )

  type MethodAttributeRefState[F[_]] =
    Fix[MethodAttributeRefStateRec[F, *]]

  type TopLevelObjectsWithMethodRefs[F[_]] =
    TopLevelObjects[F, EOObj[EOExprOnly], EOBndExpr[EOExprOnly], MethodAttributeRefState[F]]

  object effects {
    trait MethodAttribute[F[_], S] {
      def name: String
      def params: Vector[String]
      def getState: F[S]
      def referenceMethod(method: MethodAttribute[F, S]): F[Unit]
    }

    trait TopLevelObject[F[_], E, S] {
      def objName: String
      def attributes: F[Vector[MethodAttribute[F, S]]]
      def addMethodAttribute(expr: E): F[Unit]
      def findAttributeWithName(name: String): OptionT[F, MethodAttribute[F, S]]
    }

    trait TopLevelObjects[F[_], O, E, S] {
      def objects: F[Vector[TopLevelObject[F, E, S]]]
      def add(objName: String, obj: O): F[Unit]
      def findMethodsWithParamsByName(methodName: String): F[Vector[MethodAttribute[F, S]]]
    }
  }

  object programs {
    import cats._
    import cats.implicits._
    import cats.effect._
    import effects._
    import interpreters.createTopLevelObjectsWithRefs

    def resolveTopLevelObjectsAndAttrs[
      F[_]: Monad,
      S,
    ](
      eoProg: EOProg[EOExprOnly]
    )(
      implicit objs: TopLevelObjects[F, EOObj[EOExprOnly], EOBndExpr[EOExprOnly], S],
    ): F[Unit] = for {
      _ <- eoProg.bnds.traverse {
        case EOBndExpr(objName, objExpr) => Fix.un(objExpr) match {
          case o: EOObj[EOExprOnly] => objs.add(objName.name.name, o)
          case _ => implicitly[Monad[F]].pure(())
        }
        case _ => implicitly[Monad[F]].pure(())
      }
    } yield ()

    def resolveMethodsReferences[
      F[_]: Monad,
    ](
      implicit objs: TopLevelObjectsWithMethodRefs[F],
    ): F[Vector[String]] = {
      def analyzeMethodBodyExpr(
        expr: EOExprOnly,
        topLevelObjectName: String,
        methodName: String,
        methodBodyAttrName: String
      )(
        implicit methAttr: MethodAttribute[F, MethodAttributeRefState[F]]
      ): F[Vector[String]] = Fix.un(expr) match {
        case _: EOObj[EOExprOnly] => implicitly[Monad[F]].pure(Vector(
          s"""Warning: cannot analyze object
             |${topLevelObjectName}.${methodName}.${methodBodyAttrName},
             |because analysis of nested objects is not supported""".stripMargin
        ))
        case EOCopy(trg, args) => trg match {
          // if the pattern is like `self.attrName self`
          // then it is possible that attrName is recursive for some self
          // so we try to find it for some self and record this fact in the
          // method state
          case EODot(EOSimpleApp(dotLeftName), attrName)
            if args.headOption.exists(_.expr match {
              case EOSimpleApp(firstArgName) => firstArgName == dotLeftName
              case _ => false
            }) => for {
              referencedMethods <- objs.findMethodsWithParamsByName(attrName)
              _ <- referencedMethods.traverse_{ refMeth =>
                methAttr.referenceMethod(refMeth)
              }
            } yield Vector.empty
          case e => analyzeMethodBodyExpr(
            e,
            topLevelObjectName,
            methodName,
            methodBodyAttrName
          )
        }
        case EOArray(elems) => elems.flatTraverse { elem =>
          analyzeMethodBodyExpr(
            elem.expr,
            topLevelObjectName,
            methodName,
            methodBodyAttrName
          )
        }
        // Do not analyze (because they can't cause recursion):
        // - simple app
        // - just dot
        // - simple data (non-array)
        case _ => implicitly[Monad[F]].pure(Vector.empty)
      }

      for {
        objects <- objs.objects
        methods <- objects.flatTraverse { obj =>
          obj.attributes.map(_.map(ma => (ma, obj.objName)))
        }
        result <- methods.foldM(Vector.empty[String]) { (res, method) =>
          val (meth, objName) = method
          for {
            stateFixed <- meth.getState
            (_, body) = Fix.un(stateFixed)
            methBodyResult <- body.foldM(Vector.empty[String]) { (methBodyRes, methBodyAttr) =>
              for {
                methodBodyAttrResult <- methBodyAttr match {
                  case EOBndExpr(methAttrAttrName, exprToAnalyze) =>
                    analyzeMethodBodyExpr(
                      exprToAnalyze,
                      objName,
                      meth.name,
                      methAttrAttrName.name.name
                    ) (meth)
                  case _ => implicitly[Monad[F]].pure(Vector.empty[String])
                }
              } yield methodBodyAttrResult ++ methBodyRes
            }
          } yield res ++ methBodyResult
        }
      } yield result
    }

    def resolveMethodsReferencesForEOProgram[F[_]: Sync](
      program: EOProg[EOExprOnly]
    ): F[TopLevelObjectsWithMethodRefs[F]] = for {
      topLevelObjects <- createTopLevelObjectsWithRefs[F]
      _ <- resolveTopLevelObjectsAndAttrs[F, MethodAttributeRefState[F]](program)(
        implicitly,
        topLevelObjects
      )
      _ <- resolveMethodsReferences(implicitly, topLevelObjects)
    } yield topLevelObjects
  }

  object errors {
    case class DuplicatedMethodAttributes(
      val objectName: String,
      val methodAttrName: String
    ) extends Exception(
      s"""Object ${objectName} defines ${methodAttrName} multiple times."""
    )
  }

  object interpreters {
    import cats.implicits._
    import cats.effect._
    import effects._
    import errors._

    type MethodAttrWithRefs[F[_]] =
      MethodAttribute[F, MethodAttributeRefState[F]]

    type MethodAttributesWithRefs[F[_]] =
      TopLevelObject[F, EOBndExpr[EOExprOnly], MethodAttributeRefState[F]]

    type TopLevelObjectsWithRefs[F[_]] =
      TopLevelObjects[F, EOObj[EOExprOnly], EOBndExpr[EOExprOnly], MethodAttributeRefState[F]]

    def createMethodAttributeWithRefs[F[_]: Sync](
      methodName: String,
      methodParams: Vector[String],
      methodBody: Vector[EOBnd[EOExprOnly]]
    ): F[MethodAttrWithRefs[F]] = for {
      referencedMethodSet <- Sync[F].delay(
        scala.collection.mutable.Set[MethodAttribute[F, MethodAttributeRefState[F]]]()
      )
    } yield new MethodAttrWithRefs[F] {
      override def name: String = methodName

      override def params: Vector[String] = methodParams

      override def getState: F[MethodAttributeRefState[F]] =
        Sync[F].delay(Fix[MethodAttributeRefStateRec[F, *]]((
          referencedMethodSet.toSet,
          methodBody
        )))

      override def referenceMethod(
        method: MethodAttribute[F, MethodAttributeRefState[F]]
      ): F[Unit] = Sync[F].delay {
        referencedMethodSet.add(method)
        ()
      }

      // Override the comparison methods explicitly, so that it is possible to
      // compare the created object by reference, which will be needed to manipulate
      // values in sets
      override def equals(o: Any): Boolean = super.equals(o)
      override def hashCode: Int = super.hashCode
    }

    def createMethodAttributesWithRefs[
      F[_]: Sync
    ](
      objectName: String
    ): F[MethodAttributesWithRefs[F]] = for {
      attrsMap <- Sync[F].delay(
        scala.collection.mutable.Map[
          String,
          MethodAttribute[F, MethodAttributeRefState[F]]
        ]()
      )
    } yield new MethodAttributesWithRefs[F] {
      override def objName: String = objectName

      override def attributes: F[Vector[MethodAttribute[F, MethodAttributeRefState[F]]]] =
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
                methodAttr <- createMethodAttributeWithRefs(
                  methodName,
                  freeAttrs.map(_.name) ++ varargAttr.map(_.name),
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
      ): OptionT[F, MethodAttribute[F, MethodAttributeRefState[F]]] =
        OptionT(Sync[F].delay(attrsMap.get(name)))
    }

    def createTopLevelObjectsWithRefs[
      F[_]: Sync
    ]: F[TopLevelObjectsWithRefs[F]] =
      for {
        objsMap <- Sync[F].delay(
          scala.collection.mutable.Map[
            String,
            TopLevelObject[F, EOBndExpr[EOExprOnly], MethodAttributeRefState[F]]
          ]()
        )
      } yield new TopLevelObjectsWithRefs[F] {
        override def objects: F[Vector[TopLevelObject[F, EOBndExpr[EOExprOnly], MethodAttributeRefState[F]]]] =
          Sync[F].delay {
            objsMap.values.toVector
          }

        override def add(
          objName: String, obj: EOObj[EOExprOnly]
        ): F[Unit] = for {
          methodAttrs <- createMethodAttributesWithRefs[F](objName)
          _ <- obj.bndAttrs.traverse_ { objBodyAttr =>
            methodAttrs.addMethodAttribute(objBodyAttr)
          }
          _ <- Sync[F].delay {
            objsMap += (objName -> methodAttrs)
          }
        } yield ()

        override def findMethodsWithParamsByName(
          methodName: String
        ): F[Vector[MethodAttribute[F, MethodAttributeRefState[F]]]] = for {
          objects <- Sync[F].delay(objsMap.toVector.map(_._2))
          methods <- objects.flatTraverse(_.attributes)
          result = methods
            .filter(_.name == methodName)
            .filter(_.params.nonEmpty)
        } yield result
      }
  }
}
