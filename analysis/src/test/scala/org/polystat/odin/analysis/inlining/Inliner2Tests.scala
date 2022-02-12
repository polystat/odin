package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Inliner2.{createMethod, MethodInfo}
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.ToEO.instances._

object Inliner2Tests {

  def main(args: Array[String]): Unit = {
    val code =
      """[] > obj
        |  [self i] > stuff
        |    i > @
        |  [self] > method
        |    [i] > helper
        |      ^.self.stuff ^.self "method.helper" > @
        |    $.self.stuff $.self "method" > @
        |    [] > zhizha
        |      ^.self.stuff ^.self "method.zhizha" > @
        |    [] > popa
        |      [] > sisa
        |        ^.^.self.stuff ^.^.self "method.popa.sisa" > @
        |
        |  
        |""".stripMargin

    val parsedMethods: Either[String, Vector[MethodInfo]] =
      Parser
        .parse(code)
        .flatMap(prog =>
          Fix.un(prog.bnds.head.expr) match {
            case obj: EOObj[EOExprOnly] => Right(obj)
            case _ => Left("No obj")
          }
        )
        .map(_.bndAttrs.flatMap(createMethod))

    pprint.pprintln(parsedMethods)
    parsedMethods.foreach(methods =>
      methods.foreach(method =>
        method
          .calls.zipWithIndex
          .foreach { case (call, i) =>
            println(s"${i+1}." +
              call
                .callSite
                .andThen(call.callLocation)
                .getOption(method.body)
//                .replaceOption(Fix[EOExpr](EOSimpleApp("ABOBA")))(method.body)
                .map(_.toEOPretty)
            )
          }
      )
    )

  }

}
