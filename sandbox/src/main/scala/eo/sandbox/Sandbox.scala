package eo.sandbox

import cats.effect.{ ExitCode, IO, IOApp }
import cats.implicits._
import eo.analysis.mutualrec.naive.mutualrec.programs._
import eo.backend.eolang.ToEO.instances._
import eo.backend.eolang.ToEO.ops._
import eo.backend.eolang.inlineorlines.ops._
import eo.sandbox.programs.mutualRecursionExample
import higherkindness.droste.data.Fix

import scala.util.chaining._

object Sandbox extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = for {
    exitCode <- IO.pure(ExitCode.Success)
    mutualRecEORepr: String = mutualRecursionExample.toEO.allLinesToString
    _ <- IO(mutualRecEORepr.tap(println))

    topLevelObjects <- resolveMethodsReferencesForEOProgram[IO](mutualRecursionExample)

    objects <- topLevelObjects.objects
    methods <- objects.flatTraverse(obj => obj.attributes.map(_.map(meth => (obj.objName, meth))))
    methodsReferences <- methods.traverse { method =>
      val (objName, meth) = method
      for {
        methRefsFixed <- meth.getState
        methRefs = Fix.un(methRefsFixed)._1
        refsOfRefsFixed <- methRefs.toVector.traverse{ refOfRef =>
          refOfRef.getState
        }
        refsOfRefs = refsOfRefsFixed.map(rorf => Fix.un(rorf)._1)
      } yield (
        s"${objName}.${meth.name}",
        methRefs.zip(refsOfRefs).map{ mr =>
          val (meth, ref) = mr
          (meth.name, ref.map(_.name))
        }
      )
    }
    _ <- IO.suspend(
      methodsReferences.traverse_ { methRefs =>
        val (methodPath, refMethNames) = methRefs
        IO(println(
          s"${methodPath} references methods with names:\n\t" ++
            refMethNames.map { method =>
              val (meth, ref) = method
              s"${meth}, which references:\n\t\t" ++
              ref.mkString("\n\t")
            }.mkString("\n\t")
        ))
      }
    )
  } yield exitCode
}
