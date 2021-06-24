package eo.analysis.mutualrec.naive

//import cats.data.Chain
import scala.collection.immutable.Map
import eo.core.ast._

object mutualrec {
  type MethodSig[A] = Map[String, MethodAttr[A]]
  type TopLevelObjects[A] = Map[String, Object[A]]

  case class MethodAttr[A](freeAttrs: Vector[String], body: Vector[EOBndExpr[A]])
  case class Object[A](attrs: MethodSig[A])

  val assumedSelfName: String = "self"

  object effects {
    trait ObjectsResolver[F[_], A, R] {
      def resolveObj(objName: String, node: A): F[R]
    }
  }

  object programs {
    import cats._
    import cats.implicits._
    import effects._

    def resolveTopLevelObjectsAndAttrs[
      F[_] : Monad, G <: EOExpr[G]
    ](
      eoProg: EOProg[EOExpr[G]]
    )(
      implicit rslv: ObjectsResolver[F, EOObj[EOExpr[G]], TopLevelObjects[G]],
    ): F[TopLevelObjects[G]] = eoProg.bnds.foldM(Map.empty[String, Object[G]])((state, bnd) =>
      bnd match {
        case EOBndExpr(bndName, expr) => expr match {
          case o: EOObj[EOExpr[G]] => rslv.resolveObj(bndName.name.name, o)
          case _ => implicitly[Monad[F]].pure(state)
        }
        case _ => implicitly[Monad[F]].pure(state)
      }
    )

//    def findMethodRecursionForTopObjects[
//      F[_], G <: EOExpr[G]
//    ](
//      topLevelObjects: TopLevelObjects[G]
//    )(
//      implicit ae: MonadError[F, String],
//    ): F[Vector[String]] = {
////      def methBodyTraverser(
////        methExpr: EOExpr[G],
////        pathChain: Chain[String]
////      ): F[String] = ???
////        methExpr match {
////          case EOSimpleApp(_) => alt.empty
////          case d: EODot[G] => methBodyTraverser(d, pathChain)
////          case EOCopy(copyApply, copyArgs) =>
////            (copyApply, copyArgs.lift(0)) match {
////              // TODO: we found the pattern, search for the attribute `dotRightName`
////              // in the `topLevelObjects` attributes and if the body references
////              // this attribute via the same pattern - we have found a recursion
////              case (
////                EODot(EOSimpleApp(dotLeftName), dotRightName),
////                Some(EOAnonExpr(EOSimpleApp(firstParamName)))
////              ) if dotLeftName == firstParamName => ???
////            }
////          case EOObj(fr, va, bnd) => alt.pure(
////            s"""Warning: cannot analyze ${pathChain.mkString_(".")},
////               |because analysis of nested objects is not
////               |supported.""".stripMargin
////          )
////          case _ => alt.empty
////        }
//
////      val res = for {
////        (objName, obj) <- topLevelObjects.toVector
////        (methodName, methodAttr) <- obj.attrs.toVector
////        MethodAttr(methodParams, methodBody) = methodAttr
////        EOBndExpr(methodAttrName, methodAttrExpr) <- methodBody
////        pathChain = Chain(objName, methodName, methodAttrName.name.name)
////      } yield methBodyTraverser(methodAttrExpr, pathChain)
////
////      res.sequence(implicitly, alt)
//      // TODO: rewrite via foldM
//      ???
//    }
//
//    def findMethodRecursionForTopObjects[
//      F[_], G <: EOExpr[G]
//    ](
//      eoProg: EOProg[EOExpr[G]]
//    )(
//      implicit rslv: ObjectsResolver[F, EOObj[EOExpr[G]], TopLevelObjects[G]],
//      me: MonadError[F, String],
//    ): F[Vector[String]] = for {
//      topObjects <- resolveTopLevelObjectsAndAttrs(eoProg)(me, rslv)
//      res: Vector[String] <- {
//        val x = findMethodRecursionForTopObjects(topObjects)(me)
//
//        // TODO: remove, when implemented
//        me.raiseError("Not implemented, yet")
//      }
//    } yield res
  }

  object interpreters {
    import cats.MonadError
    import cats.implicits._
    import cats.data.StateT
    import effects._

    implicit def objectsResolverEitherImmutableDict[
      G <: EOExpr[G],
      M[_]
    ](
      implicit me: MonadError[M, String]
    )
    : ObjectsResolver[
      Lambda[A => StateT[M, A, A]], EOObj[EOExpr[G]], TopLevelObjects[G]
    ] = new ObjectsResolver[Lambda[A => StateT[M, A, A]], EOObj[EOExpr[G]], TopLevelObjects[G]] {
      override def resolveObj(
        objName: String,
        node: EOObj[EOExpr[G]]
      ): StateT[M, TopLevelObjects[G], TopLevelObjects[G]] = StateT[M, TopLevelObjects[G], TopLevelObjects[G]] {
        (s: TopLevelObjects[G]) =>
          if (s.keySet.contains(objName))
            me.raiseError(s"The name ${objName} is already defined")
          else {
            val result: M[(TopLevelObjects[G], TopLevelObjects[G])] = for {
              objAttrs <- node.bndAttrs.foldM[M, MethodSig[G]](Map.empty[String, MethodAttr[G]])
                { (methods, bndExpr) =>
                  val methAttrName = bndExpr.bndName.name.name
                  if (methods.contains(methAttrName))
                    me.raiseError(s"The object ${objName} defines method-attribute ${methAttrName} more than once")
                  else
                    bndExpr.expr match {
                      // Assume all method's free attributes are unique
                      case EOObj(freeAttrs, _, bndAttrs) => me.pure(
                        methods + (methAttrName -> MethodAttr(
                          freeAttrs.map(_.name),
                          bndAttrs
                        ))
                      )
                      case _ => me.pure(methods)
                    }
                }
              obj = Object(objAttrs)
              updatedTopLevelObjs = s + (objName -> obj)
            } yield (updatedTopLevelObjs, updatedTopLevelObjs)

            result
          }
      }
    }
  }
}
