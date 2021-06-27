package eo.analysis.mutualrec.naive
import cats.data._
import eo.core.ast._
import higherkindness.droste.data.Fix

object mutualrec {
  import eo.analysis.mutualrec.naive.mutualrec.effects.{ MethodAttribute, TopLevelObjects }

  type MethodAttributeRefStateRec[F[_], E <: EOExpr[E], S] = (
    scala.collection.immutable.Set[MethodAttribute[F, S]],
    Vector[EOBnd[E]]
  )

  type MethodAttributeRefState[F[_], E <: EOExpr[E]] =
    Fix[MethodAttributeRefStateRec[F, E, *]]

  type TopLevelObjectsWithMethodRefs[F[_], E <: EOExpr[E]] =
    TopLevelObjects[F, EOObj[EOExpr[E]], EOBndExpr[E], MethodAttributeRefState[F, E]]

  object effects {
    trait MethodAttribute[F[_], S] {
      def name: String
      def params: Vector[String]
      def getState: F[S]
      def referenceMethod(method: MethodAttribute[F, S]): F[Unit]
    }

    trait MethodAttributes[F[_], E, S] {
      def objName: String
      def attributes: F[Vector[MethodAttribute[F, S]]]
      def addMethodAttribute(expr: E): F[Unit]
      def findAttributeWithName(name: String): OptionT[F, MethodAttribute[F, S]]
    }

    trait TopLevelObjects[F[_], O, E, S] {
      def objects: F[Vector[MethodAttributes[F, E, S]]]
      def add(objName: String, obj: O): F[Unit]
      def findMethodsWithParamsByName(methodName: String): F[Vector[MethodAttribute[F, S]]]
    }
  }

  object programs {
    import cats._
    import cats.implicits._
    import effects._

    def resolveTopLevelObjectsAndAttrs[
      F[_]: Monad,
      E <: EOExpr[E],
      S,
    ](
      eoProg: EOProg[EOExpr[E]]
    )(
      implicit objs: TopLevelObjects[F, EOObj[EOExpr[E]], EOExpr[E], S],
    ): F[Unit] = for {
      _ <- eoProg.bnds.traverse {
        case EOBndExpr(objName, objExpr) => objExpr match {
          case o: EOObj[EOExpr[E]] => objs.add(objName.name.name, o)
          case _ => implicitly[Monad[F]].pure(())
        }
        case _ => implicitly[Monad[F]].pure(())
      }
    } yield ()

    def resolveMethodsReferences[
      F[_]: Monad,
      E <: EOExpr[E],
    ](
      implicit objs: TopLevelObjectsWithMethodRefs[F, E],
    ): F[Vector[String]] = {
      def analyzeMethodBodyExpr(
        expr: EOExpr[E],
        topLevelObjectName: String,
        methodName: String,
        methodBodyAttrName: String
      )(
        implicit methAttr: MethodAttribute[F, MethodAttributeRefState[F, E]]
      ): F[Vector[String]] = expr match {
        case _: EOObj[EOExpr[E]] => implicitly[Monad[F]].pure(Vector(
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

    type MethodAttrWithRefs[F[_], E <: EOExpr[E]] =
      MethodAttribute[F, MethodAttributeRefState[F, E]]

    type MethodAttributesWithRefs[F[_], E <: EOExpr[E]] =
      MethodAttributes[F, EOBndExpr[EOExpr[E]], MethodAttributeRefState[F, E]]

    type TopLevelObjectsWithRefs[F[_], E <: EOExpr[E]] =
      TopLevelObjects[F, EOObj[EOExpr[E]], EOBndExpr[EOExpr[E]], MethodAttributeRefState[F, E]]

    def createMethodAttribute[F[_]: Sync, E <: EOExpr[E]](
      methodName: String,
      methodParams: Vector[String],
      methodBody: Vector[EOBnd[E]]
    ): F[MethodAttrWithRefs[F, E]] = for {
      referencedMethodSet <- Sync[F].delay(
        scala.collection.mutable.Set[MethodAttribute[F, MethodAttributeRefState[F, E]]]()
      )
    } yield new MethodAttrWithRefs[F, E] {
      override def name: String = methodName

      override def params: Vector[String] = methodParams

      override def getState: F[MethodAttributeRefState[F, E]] =
        Sync[F].delay(Fix[MethodAttributeRefStateRec[F, E, *]]((
          referencedMethodSet.toSet,
          methodBody
        )))

      override def referenceMethod(
        method: MethodAttribute[F, MethodAttributeRefState[F, E]]
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

    def createMethodAttributes[
      F[_]: Sync, E <: EOExpr[E]
    ](
      objectName: String
    ): F[MethodAttributesWithRefs[F, E]] = for {
      attrsMap <- Sync[F].delay(
        scala.collection.mutable.Map[
          String,
          MethodAttribute[F, MethodAttributeRefState[F, E]]
        ]()
      )
    } yield new MethodAttributesWithRefs[F, E] {
      override def objName: String = objectName

      override def attributes: F[Vector[MethodAttribute[F, MethodAttributeRefState[F, E]]]] =
        Sync[F].delay {
          attrsMap.values.toVector
        }

      override def addMethodAttribute(
        expr: EOBndExpr[EOExpr[E]]
      ): F[Unit] = {
        val methodName = expr.bndName.name.name

        if (attrsMap.contains(methodName))
          Sync[F].raiseError(DuplicatedMethodAttributes(objectName, methodName))
        else {
          expr.expr match {
            case EOObj(freeAttrs, varargAttr, methBodyAttrs) =>
              for {
                methodAttr <- createMethodAttribute(
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
      ): OptionT[F, MethodAttribute[F, MethodAttributeRefState[F, E]]] =
        OptionT(Sync[F].delay(attrsMap.get(name)))
    }

    def createTopLevelObject[
      F[_]: Sync, E <: EOExpr[E]
    ]: F[TopLevelObjectsWithRefs[F, E]] =
      for {
        objsMap <- Sync[F].delay(
          scala.collection.mutable.Map[
            String,
            MethodAttributes[F, EOBndExpr[EOExpr[E]], MethodAttributeRefState[F, E]]
          ]()
        )
      } yield new TopLevelObjectsWithRefs[F, E] {
        override def objects: F[Vector[MethodAttributes[F, EOBndExpr[EOExpr[E]], MethodAttributeRefState[F, E]]]] =
          Sync[F].delay {
            objsMap.values.toVector
          }

        override def add(
          objName: String, obj: EOObj[EOExpr[E]]
        ): F[Unit] = for {
          methodAttrs <- createMethodAttributes[F, E](objName)
          _ <- obj.bndAttrs.traverse_ { objBodyAttr =>
            methodAttrs.addMethodAttribute(objBodyAttr)
          }
          _ <- Sync[F].delay {
            objsMap += (objName -> methodAttrs)
          }
        } yield ()

        override def findMethodsWithParamsByName(
          methodName: String
        ): F[Vector[MethodAttribute[F, MethodAttributeRefState[F, E]]]] = for {
          objects <- Sync[F].delay(objsMap.toVector.map(_._2))
          methods <- objects.flatTraverse(_.attributes)
          result = methods
            .filter(_.name == methodName)
            .filter(_.params.nonEmpty)
        } yield result
      }
  }
}
