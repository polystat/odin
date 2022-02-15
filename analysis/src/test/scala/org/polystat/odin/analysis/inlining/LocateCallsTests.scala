package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.LocateCalls.parseMethod
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.backend.eolang.ToEO.instances._

object LocateCallsTests {

  def main(args: Array[String]): Unit = {
//    val code =
//      """[] > obj
//        |  [self i] > stuff
//        |    (i.eq 0).if > @
//        |      1
//        |      i.add ($.self.stuff $.self (i.sub 1))
//        |  [self] > method
//        |    [self i] > helper
//        |      $.self.lol $.self > will-not-work
// | ^.self.stuff ^.self (^.self.stuff ^.self 123) > call-by-name
//        |      ^.self.stuff ^.self "method.helper" > @
//        |    $.self.stuff $.self "method" > @
//        |    [] > zhizha
//        |      ^.self.stuff ^.self "method.zhizha" > @
//        |    [] > foo
//        |      [] > bar
//        |        ^.^.self.stuff ^.^.self "method.foo.bar" > @
//        |    [] > more-stuff
//        |      * > @
//        |        (^.self.stuff ^.self 123).add
//        |          ^.self.stuff ^.self 234
//        |    [] > i
//        |      [] > am
//        |        [] > four
//        |          [] > and
//        |            [] > this
//        |              [] > is-as-far
//        |                [] > as-I-can-count
// | ^.^.^.^.^.^.^.self.stuff ^.^.^.^.^.^.^.self 711 > @
//        |""".stripMargin
    val code: String =
      """
        |[] > a
        |  [self y] > x
        |    $.y > @
        |
        |  [self x y] > f
        |    $.self.g $.self $.x > h
        |    [] > @
        |      ^.self.g ^.self ^.y > z
        |
        |  [self z] > g
        |    ^.x > k
        |    $.z > l
        |    [] > @
        |      ^.l > a
        |      ^.k > b
        |      ^.z > c
        |      ^.self > d
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
        .map(_.bndAttrs.flatMap(bnd => parseMethod(bnd, 0)))

    println("Before replacement:")
    println(code)

    val inlinedMethod = parsedMethods.map(methods =>
      methods.flatMap(method =>
        method
          .calls
          .foldLeft[Option[EOObj[EOExprOnly]]](Some(method.body)) {
            case (acc, call) =>
              val callPosition = call.callSite.andThen(call.callLocation)
              acc.flatMap(
                callPosition
                  .replaceOption(Fix[EOExpr](EOSimpleApp("REDACTED")))(_)
              )
          }
      )
    )

    println("After replacing each valid call with REDACTED:")
    inlinedMethod.foreach(_.foreach(obj => println(obj.toEOPretty)))

    println(
      """Method bodies after adding 
        |an attribute 'lol' with value "hello there"
        |to each call site.
        |""".stripMargin
    )
    parsedMethods.foreach(methods =>
      methods.foreach(method =>
        method
          .calls
          .foreach(call => {
            println(
              call
                .callSite
                .modifyOption(obj =>
                  obj.copy(bndAttrs =
                    obj.bndAttrs :+ EOBndExpr(
                      EOAnyNameBnd(LazyName("lol")),
                      Fix[EOExpr](EOStrData("Hello there"))
                    )
                  )
                )(method.body)
                .map(_.toEOPretty)
            )
            println("________________________")
          })
      )
    )

  }

}
