package org.polystat.odin.analysis.inlining

import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Context.setLocators
import org.polystat.odin.core.ast._
import org.polystat.odin.parser.eo.Parser
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops.ToEOOps
import cats.syntax.traverse._
import cats.syntax.foldable._

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

  def inlineHelper(
    argMap: Map[String, EOBnd[Fix[EOExpr]]],
    argsObjName: String,
    bnd: EOBnd[Fix[EOExpr]],
    depthOffset: BigInt,
    is_phi: Boolean = false
  ): EOBnd[Fix[EOExpr]] = {

    def exprHelper(
      depth: BigInt,
      is_arg: Boolean = false,
    )(
      expr: EOExpr[Fix[EOExpr]],
    ): EOExpr[Fix[EOExpr]] = expr match {
      case copy @ EOCopy(Fix(trg), args) =>
        copy.copy(
          trg = Fix(exprHelper(depth)(trg)),
          args = args.map(bndHelper(depth))
        )
      case dot @ EODot(Fix(src), _) =>
        dot.copy(src = Fix(exprHelper(depth)(src)))

      // TODO: add proper processing for anonymous objects passed as arguments
      case obj @ EOObj(_, _, bnds) if !is_arg =>
        obj.copy(
          bndAttrs = bnds.map(bndExprHelper(depth + 1))
        )

      case app @ EOSimpleAppWithLocator(_, depth) if is_arg =>
        app.copy(locator = depth + 1)

      case app @ EOSimpleAppWithLocator(name, locator) =>
        // Changing locator to account for additional nestedness
        if (is_arg) {
          app.copy(locator = locator + depthOffset + depth)
        } else
          argMap.get(name) match {
            // Is an argument -> replace with passed value and process further
            case Some(bnd) =>
              exprHelper(depth + locator, is_arg = true)(Fix.un(bnd.expr))
            // Not an argument
            case None =>
              // Make application refer to the argsObj, as it is in ohi
              if (is_phi) {
                val argsObj =
                  EOSimpleAppWithLocator[Fix[EOExpr]](argsObjName, depth)
                EODot(Fix(argsObj), name)
              } else {
                println(depth)
                app.copy(locator = locator + depthOffset + depth)
              }
          }

      case other =>
        other
    }

    def bndExprHelper(depth: BigInt)(
      bnd: EOBndExpr[Fix[EOExpr]]
    ): EOBndExpr[Fix[EOExpr]] =
      bnd.copy(expr = Fix(exprHelper(depth)(Fix.un(bnd.expr))))

    def bndHelper(depth: BigInt)(bnd: EOBnd[Fix[EOExpr]]): EOBnd[Fix[EOExpr]] =
      bnd match {
        case bnd @ EOAnonExpr(Fix(expr)) =>
          bnd.copy(expr = Fix(exprHelper(depth)(expr)))
        case bnd @ EOBndExpr(_, _) => bndExprHelper(depth)(bnd)
      }

    bndHelper(0)(bnd)
  }

  def inlineArgs(
    call: EOCopy[Fix[EOExpr]],
    methodBody: EOObj[Fix[EOExpr]],
    argsObjName: String,
    depthOffset: BigInt
  ): Vector[EOBndExpr[Fix[EOExpr]]] = {
    val argMap = methodBody.freeAttrs.map(_.name).zip(call.args).toMap

    methodBody
      .bndAttrs
      .map {
        case bnd @ EOBndExpr(EODecoration, _) =>
          inlineHelper(argMap, argsObjName, bnd, depthOffset, is_phi = true)
        case bnd => inlineHelper(argMap, argsObjName, bnd, depthOffset)
      }
      .collect { case value: EOBndExpr[Fix[EOExpr]] => value }
  }

  def inlineCall(
    method: Method,
    call: EOCopy[Fix[EOExpr]],
    argsObjName: String,
    bndName: EONamedBnd,
    depthOffset: BigInt
  ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {

    val newMethodBody = inlineArgs(call, method.body, argsObjName, depthOffset)
    val phi = newMethodBody
      .collectFirst { case EOBndExpr(EODecoration, expr) =>
        expr
      }
      .map(expr => EOBndExpr(bndName = bndName, expr = expr))

    phi match {
      case Some(phiBnd) =>
        val bodyWithoutPhi =
          newMethodBody.filter(_ != EOBndExpr(EODecoration, phiBnd.expr))

        if (bodyWithoutPhi.isEmpty) {
          Right(Vector(phiBnd))
        } else {
          val argsObj = EOBndExpr(
            EOAnyNameBnd(LazyName(argsObjName)),
            Fix(EOObj(Vector.empty, None, bodyWithoutPhi))
          )
          Right(Vector(argsObj, phiBnd))
        }

      case None =>
        Left(s"Method ${method.name} does not have a Phi attribute")
    }
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
    ): Vector[String] = {

      def exprHelper(
        expr: Fix[EOExpr],
        upperBndName: Option[String]
      ): Option[String] = Fix.un(expr) match {
        case EOCopy(trg, args) =>
          exprHelper(trg, upperBndName)
            .orElse(
              args.value.foldMapK(bndHelper(upperBndName))
            )
        case EODot(trg, _) =>
          exprHelper(trg, upperBndName)
        case EOObj(_, _, bnds) => bnds.foldMapK(bndHelper(upperBndName))
        case EOSimpleAppWithLocator("@", _) =>
          upperBndName
        case _ =>
          None
      }

      def bndHelper(
        upperBndName: Option[String]
      )(bnd: EOBnd[Fix[EOExpr]]): Option[String] =
        bnd match {
          case EOAnonExpr(expr) => exprHelper(expr, upperBndName)
          case EOBndExpr(_, expr) => exprHelper(expr, upperBndName)
        }

      binds.flatMap {
        case EOAnonExpr(_) => None
        case EOBndExpr(bndName, expr) =>
          exprHelper(expr, Some(bndName.name.name))
      }

    }

    def checkCallValidity(
      bnd: EOBndExpr[Fix[EOExpr]],
      call: EOCopy[Fix[EOExpr]],
      locator: BigInt,
      method: Method
    ): Either[String, Vector[EOBndExpr[Fix[EOExpr]]]] = {
      val callHasProperNumOfArgs =
        call.args.length == method.body.freeAttrs.length

      // -1 accounts for the additional depth of the method
      val callHasCorrectDepth =
        availableMethods.depth == currentDepth - locator - 1

      val callHasCorrectSelfInArgs = call.args.headOption match {
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
        case (true, true, true, names) if names.nonEmpty =>
          Left(
            s"Attached attributes ${names.mkString(", ")} of method ${method.name} use 𝜑."
          )

        // The call matches all criteria -> needs inlining
        case (true, true, true, _) =>
          val callNumber = binds.indexOf(bnd)
          val argsObjName = baseArgsObjName(method.name) + "_" + callNumber
          // Used for tracking how deep the resulting phi will be located
          val depthOffset = availableMethods.depth + locator

          inlineCall(method, call, argsObjName, bnd.bndName, depthOffset)
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
      val newDepth = currentDepth + 1

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
