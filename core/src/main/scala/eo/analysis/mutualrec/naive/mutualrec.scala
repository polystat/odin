package eo.analysis.mutualrec.naive

import cats._
import cats.data.{ Chain, OptionT }
import cats.effect._
import cats.implicits._
import eo.analysis.mutualrec.naive.services.MethodAttribute.MethodInfo
import eo.analysis.mutualrec.naive.services.TopLevelObjects.createTopLevelObjectsWithRefs
import eo.analysis.mutualrec.naive.services.{ MethodAttribute, TopLevelObjects }
import eo.core.ast._
import eo.core.ast.astparams.EOExprOnly
import higherkindness.droste.data.Fix

object mutualrec {
    def resolveTopLevelObjectsAndAttrs[F[_]: Monad](
      eoProg: EOProg[EOExprOnly]
    )(
      implicit objs: TopLevelObjects[F],
    ): F[Unit] = for {
      _ <- eoProg.bnds.traverse {
        case EOBndExpr(objName, objExpr) => Fix.un(objExpr) match {
          case o: EOObj[EOExprOnly] => objs.add(objName.name.name, o)
          case _ => Monad[F].pure(())
        }
        case _ => Monad[F].pure(())
      }
    } yield ()

    def resolveMethodsReferences[F[_]: Monad](
      implicit objs: TopLevelObjects[F],
    ): F[Vector[String]] = {
      def analyzeMethodBodyExpr(
        expr: EOExprOnly,
        methodName: String,
        methodBodyAttrName: String
      )(
        implicit methAttr: MethodAttribute[F]
      ): F[Vector[String]] = {
        val currentObject = methAttr.parentObject
        val currentObjectName = currentObject.objName

        Fix.un(expr) match {
          case _: EOObj[EOExprOnly] => implicitly[Monad[F]].pure(Vector(
            s"""Warning: cannot analyze object
               |${currentObjectName}.${methodName}.${methodBodyAttrName},
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
              }) =>
              val methodReferenceResult = for {
                referencedMethod <- currentObject.findAttributeWithName(attrName)
                _ <- OptionT.liftF(methAttr.referenceMethod(referencedMethod))
              } yield Vector.empty[String]
              methodReferenceResult.getOrElse(Vector.empty[String])
            case e => analyzeMethodBodyExpr(
              e,
              methodName,
              methodBodyAttrName
            )
          }
          case EOArray(elems) => elems.flatTraverse { elem =>
            analyzeMethodBodyExpr(
              elem.expr,
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
      }

      for {
        objects <- objs.objects
        methods <- objects.flatTraverse(_.attributes)
        result <- methods.foldM(Vector.empty[String]) { (res, method) =>
          for {
            MethodInfo(body, _) <- method.getMethodInfo
            methBodyResult <- body.foldM(Vector.empty[String]) { (methBodyRes, methBodyAttr) =>
              for {
                methodBodyAttrResult <- methBodyAttr match {
                  case EOBndExpr(methAttrAttrName, exprToAnalyze) =>
                    analyzeMethodBodyExpr(
                      exprToAnalyze,
                      method.name,
                      methAttrAttrName.name.name
                    )(method)
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
    ): F[TopLevelObjects[F]] = for {
      topLevelObjects <- createTopLevelObjectsWithRefs[F]
      _ <- resolveTopLevelObjectsAndAttrs[F](program)(
        implicitly,
        topLevelObjects
      )
      _ <- resolveMethodsReferences(implicitly, topLevelObjects)
    } yield topLevelObjects

    type MethodCallStack[F[_]] = Chain[MethodAttribute[F]]

    type MethodRecursiveDependency[F[_]] = Map[
      MethodAttribute[F],
      Chain[MethodCallStack[F]], // list of paths that lead to recursion
    ]

    def findMethodRecursiveLinks[F[_]: Monad](
      method: MethodAttribute[F],
      callStack: MethodCallStack[F] = Chain.empty
    ): F[MethodRecursiveDependency[F]] = {
      if (callStack.headOption.contains(method)) {
        // if the current method to inspect is the one that we have started
        // traversal from - then there is a loop in dependency graph and we
        // came to where we have started, i. e. there is a recursion, so return
        implicitly[Monad[F]].pure(Map(method -> Chain(callStack)))
      } else {
        // if the current method is different from the one we started from
        // traverse all dependencies that we didn't encounter before.
        for {
          methodInfo <- method.getMethodInfo
          MethodInfo(_, references) = methodInfo
          // If there is a dependency that is already in a call stack - there
          // is a recursion within the tree, but it does not affect the initial
          // method we are actually analyzing. (a flag can be introduced to the
          // function parameters to also include these results)

          // remove the first element from the call stack, so that it is
          // possible for it to proceed in the recursive calls of
          // [[findMethodRecursiveLinks]] and get to the base case
          callStackSet = callStack.toList.drop(1).toSet
          dependenciesToInspect = references.filterNot(callStackSet)
          recDeps = for {
            methodToInspect <- dependenciesToInspect
            resF = findMethodRecursiveLinks(
              methodToInspect,
              callStack.append(method)
            )
          } yield resF
          res <- recDeps.foldLeft(implicitly[Monad[F]].pure(Map.empty[
            MethodAttribute[F],
            Chain[MethodCallStack[F]],
          ])) { (resMapF, recDepF) =>
            for {
              resMap <- resMapF
              recDep <- recDepF
            } yield resMap.alignMergeWith(recDep)(_ ++ _)
          }
        } yield res
      }
    }

    def findMutualRecursionInTopLevelObjects[F[_]: Monad](
      topLevelObjectsWithRefs: TopLevelObjects[F]
    ): F[Vector[MethodRecursiveDependency[F]]] = {
      for {
        objects <- topLevelObjectsWithRefs.objects
        methods <- objects.flatTraverse(_.attributes)
        recDepsForMethods = for {
          method <- methods
        } yield findMethodRecursiveLinks(method)
        recDeps <- recDepsForMethods.sequence
      } yield recDeps
    }
}
