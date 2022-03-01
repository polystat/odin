package org.polystat.odin.analysis.logicalexprs

import org.polystat.odin.parser.eo.Parser

import scala.annotation.unused
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.{MethodInfo, ObjectInfo, ParentInfo}
//import org.polystat.odin.backend.eolang.ToEO.instances._
//import org.polystat.odin.backend.eolang.ToEO.ops._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.core.ast._
import org.polystat.odin.analysis.inlining.Context.setLocators

object ExtractLogic {

  type ObjInfo = ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]

  //  def extractAllMethodProperties(obj: ObjInfo): Map[MethodInfo, LMethod] = ???

  val supportedOps: Map[String, (LExpr, LExpr) => LExpr] = Map(
    "div" -> LDiv.apply,
    "add" -> LAdd.apply,
    "less" -> LLess.apply,
    "greater" -> LGreater.apply,
    "eq" -> LEq.apply
  )

  def processMethod(method: MethodInfo, @unused availableMethods: Map[MethodInfo, LMethod]): Either[String, LMethod] = {

    def extractLogic
    (@unused depth: BigInt, @unused lastBnd: Option[EOBndExpr[EOExprOnly]], @unused accPrefix: String = "")
    (expr: EOExprOnly): Either[String, LExpr] = Fix.un(expr) match {
//      case EOObj(freeAttrs, varargAttr, bndAttrs) => ???

      case EODot(expr, name) =>
        extractLogic(depth, lastBnd,name + accPrefix)(expr)

      case obj@EOObj(Vector(), _, bndAttrs) =>
//        val phi = bndAttrs
//          .collectFirst { case EOBndExpr(EODecoration, expr) => expr }
//          .map(extractLogic(0, None))
        val nonPhi = bndAttrs.filter{
          case EOBndExpr(EODecoration, _) => false
          case _ => true
        }

        for {
//          phi <- phi.getOrElse(Left("The given method has no phi attribute!"))
          attrs <- nonPhi.traverse(bnd => {
            val name = bnd.bndName.name.name
            val expr = extractLogic(method.depth, Some(bnd))(bnd.expr)

            expr.map(AttrBnd(_, name))
          })
          name <- lastBnd.toRight(s"No name found for $obj").map(_.bndName.name.name)
        } yield LObj(attrs.toList, None, name)


      case copy@EOCopy(EOSimpleAppWithLocator("assert", _), args) =>
        for {
          onlyArg <-
            if (args.length == 1)
              Right(args.head.expr)
            else
              Left(s"Wrong number of arguments given in $copy")
          expr <- extractLogic(depth, lastBnd, accPrefix)(onlyArg)
        } yield LAssert(expr)

      // Todo ???
      //      case EODot(src, "sqrt") =>

      case EOCopy(EOSimpleAppWithLocator("seq", _), args) =>
        args
          .value
          .toList
          .traverse(bnd => extractLogic(depth, lastBnd, accPrefix)(bnd.expr))
          .map(LSeq.apply)

      case copy@EOCopy(Fix(EODot(src, op)), args) =>
        for {
          // Todo probably leave only 'args.head.expr'
          onlyArg <-
            if (args.length == 1)
              Right(args.head.expr)
            else
              Left(s"Wrong number of arguments given in $copy")
          leftExpr <- extractLogic(depth, lastBnd, accPrefix)(src)
          rightExpr <- extractLogic(depth, lastBnd, accPrefix)(onlyArg)
          res <- supportedOps
            .get(op)
            .map(operation => Right(operation(leftExpr, rightExpr)))
            .getOrElse(Left(s"Unsupported operation $op"))
        } yield res

      case EOSimpleAppWithLocator(name, _) =>
        Right(Attr(name + accPrefix))
      case EOIntData(int) => Right(LConst(int))
      //      case app: EOApp[_] => ???
      //      case data: EOData[_] => ???
      case expr => Left(s"Unsupported expression $expr")
    }


    val body = method.body
    val phi = body.bndAttrs
      .collectFirst { case EOBndExpr(EODecoration, expr) => expr }
      .map(extractLogic(0, None))
    val nonPhi = body.bndAttrs.filter{
      case EOBndExpr(EODecoration, _) => false
      case _ => true
    }

    for {
      phi <- phi.getOrElse(Left("The given method has no phi attribute!"))
      attrs <- nonPhi.traverse(bnd => {
        val name = bnd.bndName.name.name
        val expr = extractLogic(method.depth, Some(bnd))(bnd.expr)

        expr.map(AttrBnd(_, name))
      })
      args = body.freeAttrs.map(name => Attr(name.name)).toList
    } yield LMethod(attrs.toList, args, phi, "Analyzed Method")
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |[self a] > method
        |  [] > inner
        |    [] > inner
        |      22 > x
        |    2 > attr
        |  (a.add 1).add inner.inner.x > y
        |  100.add 1 > x
        |  x.eq y > @
        |""".stripMargin
    //      """
    //        |[] > obj
    //        |  [self y] > g
    //        |    3.div $.y  > @
    //        |
    //        |  [self z] > method
    //        |    $.self.g $.self z > x
    //        |    $.x > @
    //        |
    //        |[] > derived
    //        |
    //        |  obj > @
    //        |  [self y] > g
    //        |    3.div ($.y.add 1) > @
    //        |
    //        |""".stripMargin

    println(Parser
      .parse(code)
      .flatMap(setLocators)
      .map(prog => {
        val method = MethodInfo(Vector(), prog.bnds(0).expr.asInstanceOf[EOObj[EOExprOnly]], 0)
        processMethod(method, Map())
          .map(_.properties(""))
      }
      )
      .merge
    )
    //      .leftMap(println)
    //      .map(println)
    //      .merge
  }
}
