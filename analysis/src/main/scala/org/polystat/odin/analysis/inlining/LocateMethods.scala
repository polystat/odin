package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.analysis.inlining.types._
import org.polystat.odin.parser.eo.Parser

object LocateMethods {

  def parseObject(obj: EOBnd[EOExprOnly]): Option[Object] = {
    type BndInfo = (
      Map[EONamedBnd, MethodInfo],
      Vector[Either[MethodPlaceholder, EOBndExpr[EOExprOnly]]]
    )
    obj match {
      case EOBndExpr(bndName, Fix(EOObj(Vector(), None, bnds))) =>
        val (methods, otherBnds) = bnds.foldLeft[BndInfo]((Map(), Vector())) {
          case ((methods, otherBnds), next) =>
            LocateCalls.createMethod(next) match {
              case Some(value) => (
                  methods.updated(next.bndName, value),
                  otherBnds.appended(
                    Left(next.bndName)
                  )
                )
              case None => (methods, Vector(Right(next)))
            }
        }

        Some(
          Object(
            name = bndName,
            methods = methods,
            bnds = otherBnds
          )
        )

      case _ => None
    }
  }

  def main(args: Array[String]): Unit = {
    val code: String =
      """
        |[] > a
        |  [self y] > x
        |    y > @
        |
        |  [self x y] > f
        |    self.g self x > h
        |    [] > @
        |      self.g self y > z
        |
        |  [self z] > g
        |    x > k
        |    z > l
        |    [] > @
        |      l > a
        |      k > b
        |      z > c
        |      self > d
        |""".stripMargin

    Parser
      .parse(code)
      .map(setLocators) match {
      case Left(value) => println(value)
      case Right(prog) =>
        prog.bnds.flatMap(parseObject).foreach(_.methods.foreach(println))
    }
  }

}
