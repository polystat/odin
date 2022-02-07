package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.instances.progToEO
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps
import cats.syntax.traverse._

import scala.annotation.tailrec

object Inliner {
  case class Method(name: String, body: EOObj[Fix[EOExpr]])
  case class MethodList(methods: List[Method], depth: BigInt)

  def extractMethods(
    obj: EOObj[Fix[EOExpr]],
    depth: BigInt
  ): MethodList = {
    val methods = obj
      .bndAttrs
      .flatMap {
        // Extracting method name
        case EOBndExpr(EOAnyNameBnd(LazyName(methodName)), Fix(expr)) =>
          expr match {
            // Checking that method has self as the first argument
            case method @ EOObj(LazyName("self") +: _, _, bndAttrs) =>
              bndAttrs
                .find {
                  // Checking if method has a Phi attribute
                  case EOBndExpr(EODecoration, _) => true
                  case _ => false
                }
                .map(_ => Method(methodName, method))
            case _ => None
          }
        case _ => None
      }
      .toList

    MethodList(methods, depth)
  }

  def propagateArgsIntoBody(
    call: EOCopy[Fix[EOExpr]],
    methodBody: EOObj[Fix[EOExpr]]
  ): Vector[EOBndExpr[Fix[EOExpr]]] = {
    println(call)

    methodBody.bndAttrs
  }

  def splitMethodBody(
    methodBody: Vector[EOBndExpr[Fix[EOExpr]]],
    argsObjName: String,
    phiName: EONamedBnd
  ): Vector[EOBndExpr[Fix[EOExpr]]] = {
    val phi =
      methodBody
        .find {
          case EOBndExpr(EODecoration, _) => true
          case _ => false
        }
        .get
        .copy(bndName = phiName)

    val tmp = methodBody.filter {
      case EOBndExpr(EODecoration, _) => false
      case _ => true
    }

    val argsObj = EOBndExpr(
      EOAnyNameBnd(LazyName(argsObjName)),
      Fix(EOObj(Vector.empty, None, tmp))
    )

    Vector(argsObj, phi)
  }

  def inlineCall(
    method: Method,
    call: EOCopy[Fix[EOExpr]],
    argsObjName: String,
    bndName: EONamedBnd
  ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {
    // Todo make it so phi is searched for only once
    val hasPhi = method
      .body
      .bndAttrs
      .exists {
        case EOBndExpr(EODecoration, _) => true
        case _ => false
      }

    if (hasPhi) {
      val newMethodBody = propagateArgsIntoBody(call, method.body)
      val argsObjAndPhi = splitMethodBody(newMethodBody, argsObjName, bndName)

      Right(argsObjAndPhi)
    } else
      Left(s"Method ${method.name} does not have a Phi attribute")
  }

  def tryInlineCalls(
    availableMethods: MethodList,
    currentDepth: BigInt
  )(
    binds: Vector[EOBndExpr[Fix[EOExpr]]]
  ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {

    def baseArgsObjName(methodName: String) = {
      val usedNames =
        binds.collect { case EOBndExpr(EOAnyNameBnd(bnd: BndName), _) =>
          bnd.name
        }

      @tailrec
      def getName(name: String): String =
        if (usedNames.exists(_.startsWith(name))) getName(name + "_") else name

      getName(s"local_$methodName")
    }

    def getAttrNamesWithPhi(
      binds: Vector[EOBnd[Fix[EOExpr]]]
    ): Option[Vector[String]] = {

      def exprHelper(
        upperBndName: String,
        expr: EOExpr[Fix[EOExpr]]
      ): Option[String] = expr match {
        // TODO: possibly add even more advanced checks for Objs and etc
        case EOCopy(EOSimpleAppWithLocator("@", _), _) |
             EODot(EOSimpleAppWithLocator("@", _), _) |
             EOSimpleAppWithLocator("@", _) =>
          Some(upperBndName)
        case EOCopy(_, innerBinds)
             if getAttrNamesWithPhi(
               innerBinds.value
             ).nonEmpty => Some(upperBndName)
        case _ => None
      }

      def bndHelper(
        bnd: EOBnd[Fix[EOExpr]],
        upperBndName: String
      ): Option[String] = bnd match {
        case EOAnonExpr(Fix(expr)) => exprHelper(upperBndName, expr)
        case EOBndExpr(bndName, Fix(expr)) =>
          exprHelper(bndName.name.name, expr)
      }

      Option(
        binds
          .flatMap(bnd =>
            bndHelper(bnd, "YOU SHOULD NOT BE SEEING THIS MESSAGE")
          )
      ).filter(_.nonEmpty)
    }

    def checkCallValidity(
      bnd: EOBndExpr[Fix[EOExpr]],
      call: EOCopy[Fix[EOExpr]],
      locator: BigInt,
      method: Method
    ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {
      lazy val callHasProperNumOfArgs =
        call.args.length == method.body.freeAttrs.length

      // -1 accounts for the additional depth of the method
      lazy val callHasCorrectDepth =
        availableMethods.depth == currentDepth - locator - 1

      lazy val callHasCorrectSelfInArgs = call.args.headOption match {
        case Some(EOAnonExpr(EOSimpleAppWithLocator("self", loc)))
             if loc == locator => true
        case _ => false
      }

      (
        callHasCorrectDepth,
        callHasCorrectSelfInArgs,
        callHasProperNumOfArgs,
        // Checking that attached attributes method to inline do not use 𝜑
        getAttrNamesWithPhi(method.body.bndAttrs)
      ) match {
        // The bnd does not require inlining
        case (false, _, _, _) | (_, false, _, _) => Right(Vector(bnd))

        // Possible errors
        case (true, true, false, _) =>
          Left(s"Wrong number of arguments given for method ${method.name}.")
        case (true, true, true, Some(names)) =>
          Left(
            s"Attached attributes ${names.mkString(", ")} of method ${method.name} use 𝜑."
          )

        // The call matches all criteria -> needs inlining
        case (true, true, true, None) =>
          val callNumber = binds.indexOf(bnd)
          val argsObjName = baseArgsObjName(method.name) + "_" + callNumber

          inlineCall(method, call, argsObjName, bnd.bndName)
      }

    }

    def processIfCall(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] =
      bnd match {
        case EOBndExpr(
               _,
               Fix(
                 call @ EOCopy(
                   EODot(EOSimpleAppWithLocator("self", locator), methodName),
                   _
                 )
               )
             ) =>
          availableMethods
            .methods
            .find(method => method.name == methodName)
            .map(method => checkCallValidity(bnd, call, locator, method))
            .getOrElse(
              Left(s"Attempt to call non-existent method $methodName.")
            )

        case other => Right(Vector(other))
      }

    binds.flatTraverse(processIfCall)
  }

  def inlineCalls(
    prog: EOProg[Fix[EOExpr]]
  ): Either[String, EOProg[Fix[EOExpr]]] = {

    def processObj(
      availableMethods: MethodList,
      newDepth: BigInt,
      obj: EOObj[Fix[EOExpr]]
    ): Either[String, EOObj[Fix[EOExpr]]] = {
      obj
        .bndAttrs
        .traverse(bndExprHelper(availableMethods, newDepth))
        .flatMap(tryInlineCalls(availableMethods, newDepth))
        .map(bnds => obj.copy(bndAttrs = bnds))
    }

    def exprHelper(
      availableMethods: MethodList,
      currentDepth: BigInt,
      isPhi: Boolean
    )(
      expr: EOExpr[Fix[EOExpr]]
    ): Either[String, EOExpr[Fix[EOExpr]]] = {
      lazy val newDepth = currentDepth + 1

      expr match {
        case method @ EOObj(LazyName("self") +: _, _, _) =>
          processObj(availableMethods, newDepth, method)

        case obj @ EOObj(Vector(), _, _) =>
          if (isPhi)
            processObj(availableMethods, newDepth, obj)
          else
            processObj(extractMethods(obj, newDepth), newDepth, obj)

        case dot @ EODot(Fix(src), _) =>
          exprHelper(availableMethods, currentDepth, isPhi)(src).map(src =>
            dot.copy(src = Fix(src))
          )

        case other => Right(other)
      }
    }

    def bndExprHelper(
      availableMethods: MethodList,
      currentDepth: BigInt
    )(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): Either[String, EOBndExpr[Fix[EOExpr]]] = {
      val isPhi = bnd.bndName == EODecoration

      exprHelper(availableMethods, currentDepth, isPhi)(Fix.un(bnd.expr)).map(
        expr => bnd.copy(expr = Fix(expr))
      )
    }

    def bndHelper(
      availableMethods: MethodList,
      currentDepth: BigInt
    )(
      bnd: EOBnd[Fix[EOExpr]]
    ): Either[String, EOBnd[Fix[EOExpr]]] =
      bnd match {
        case EOAnonExpr(Fix(expr)) =>
          exprHelper(availableMethods, currentDepth, isPhi = false)(expr).map(
            value => EOAnonExpr(Fix(value))
          )
        case bnd: EOBndExpr[Fix[EOExpr]] =>
          bndExprHelper(availableMethods, currentDepth)(bnd)
      }

    prog
      .bnds
      .traverse(bndHelper(MethodList(List.empty, 0), 0))
      .map(bnds => prog.copy(bnds = bnds))
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
//      """
//        |
//        |[] > outer
//        |  256 > magic
//        |  [] > dummy
//        |    [self] > bMethod
//        |      22 > @
//        |    [self outer] > innerMethod
//        |      [self] > innerInnerMethod
//        |        ^.self.bMethod ^.self > @
//        |      self.bMethod self > @
//        |
//        |    $.innerMethod 1 1 > b
//        |  self "yahoo" > @
//        |  [self] > method
//        |    self.magic > @
//        |
//        |""".stripMargin

    Parser
      .parse(code)
      .map(setLocators) match {
      case Left(value) => println(value)
      case Right(value) => inlineCalls(value) match {
          case Left(value) => println(value)
          case Right(value) => println(value.toEOPretty)
        }
    }
  }

}
