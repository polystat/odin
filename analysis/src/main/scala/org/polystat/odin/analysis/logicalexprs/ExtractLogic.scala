package org.polystat.odin.analysis.logicalexprs

import ap.SimpleAPI
import cats.data.{EitherNel, NonEmptyList => Nel}
import cats.syntax.align._
import cats.syntax.either._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Inliner.{AnalysisInfo, zipMethodsWithTheirInlinedVersionsFromParent}
import org.polystat.odin.analysis.inlining.{MethodInfoForAnalysis, ObjectTree}
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Parser
import smtlib.printer.RecursivePrinter
import smtlib.trees.Commands._
import smtlib.trees.Terms.Forall
import org.polystat.odin.backend.eolang.ToEO.instances._
import org.polystat.odin.backend.eolang.ToEO.ops._
import java.io.StringReader
import scala.annotation.unused


object ExtractLogic {

  //  type ObjInfo = ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]

  //  def extractAllMethodProperties(obj: ObjInfo): Map[MethodInfo, LMethod] = ???

  val supportedOps: Map[String, (LExpr, LExpr) => LExpr] = Map(
    "div" -> LDiv.apply,
    "add" -> LAdd.apply,
    "sub" -> LSub.apply,
    "less" -> LLess.apply,
    "greater" -> LGreater.apply,
    "eq" -> LEq.apply
  )

  def processMethod(method: MethodInfoForAnalysis, name: String, availableMethods: Map[EONamedBnd, LMethod]): EitherNel[String, LMethod] = {

    def extractLogic
    (@unused depth: BigInt, @unused lastBnd: Option[EOBndExpr[EOExprOnly]], accPrefix: String = "")
    (expr: EOExprOnly): EitherNel[String, LExpr] = Fix.un(expr) match {
      //      case EOObj(freeAttrs, varargAttr, bndAttrs) => ???

      case EODot(expr, name) =>
        extractLogic(depth, lastBnd, name + accPrefix)(expr)

      case obj@EOObj(Vector(), _, bndAttrs) =>
        //        val phi = bndAttrs
        //          .collectFirst { case EOBndExpr(EODecoration, expr) => expr }
        //          .map(extractLogic(0, None))
        val nonPhi = bndAttrs.filter {
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
          name <- lastBnd.toRight(Nel.one(s"No name found for $obj")).map(_.bndName.name.name)
        } yield LObj(attrs.toList, None, name)


      case copy@EOCopy(EOSimpleAppWithLocator("assert", _), args) =>
        for {
          onlyArg <-
            if (args.length == 1)
              Right(args.head.expr)
            else
              Left(Nel.one(s"Wrong number of arguments given in $copy"))
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

      case copy@EOCopy(Fix(EODot(src, op)), args) if supportedOps.contains(op) =>
        for {
          // Todo probably leave only 'args.head.expr'
          onlyArg <-
            if (args.length == 1)
              Right(args.head.expr)
            else
              Left(Nel.one(s"Wrong number of arguments given in $copy"))
          leftExpr <- extractLogic(depth, lastBnd, accPrefix)(src)
          rightExpr <- extractLogic(depth, lastBnd, accPrefix)(onlyArg)
          res <- supportedOps
            .get(op)
            .map(operation => Right(operation(leftExpr, rightExpr)))
            .getOrElse(Left(Nel.one(s"Unsupported operation $op")))
        } yield res

      case EOCopy(Fix(EODot(EOSimpleAppWithLocator("self", _), methodName)), args) =>
        for {
          processedArgs <- args
            .value
            .toList
            // TODO properly add methodname
            .traverse(b => extractLogic(depth, lastBnd, accPrefix)(b.expr))
          expectedMethod <-
            availableMethods
              .get(EOAnyNameBnd(LazyName(methodName)))
              .toRight(Nel.one(s"Attempt to call non-existent method $methodName"))
        } yield LCall(expectedMethod, processedArgs)

      case EOSimpleAppWithLocator(name, _) =>
        Right(Attr(name + accPrefix))
      case EOIntData(int) => Right(LConst(int))
      //      case app: EOApp[_] => ???
      //      case data: EOData[_] => ???
      case expr => Left(Nel.one(s"Unsupported expression $expr"))
    }


    val body = method.body
    val phi = body.bndAttrs
      .collectFirst { case EOBndExpr(EODecoration, expr) => expr }
      .map(extractLogic(0, None))
    val nonPhi = body.bndAttrs.filter {
      case EOBndExpr(EODecoration, _) => false
      case _ => true
    }

    for {
      attrs <- nonPhi.traverse(bnd => {
        val name = bnd.bndName.name.name
        val expr = extractLogic(method.depth, Some(bnd))(bnd.expr)

        expr.map(AttrBnd(_, name))
      })
      tmp <- phi.toRight(Nel.one("The given method has no phi attribute!"))
      phiExpr <- tmp
      args = body.freeAttrs.map(name => Attr(name.name)).toList
    } yield LMethod(attrs.toList, args, phiExpr, name)
  }

  def checkImplication(@unused methodBefore: LMethod, @unused methodAfter: LMethod): EitherNel[String, String] = {
//    import smtlib.theories.Core._

    val props1 = methodBefore.properties("")
//    val props2 = methodAfter.properties("")

//    val implication = Assert(Equals(Implies(props1, props2), False()))
//    val implication   = Implies(props1, props2)
      val implication = props1

//        val a = Equals(props1, True())
//        val b = Equals(props2, False())
//        val c = Equals(And(a,b),False())
//        val implication = Assert(c)


    val allArgs = methodAfter.argumentSortedVars(methodAfter.name) ++ methodBefore.argumentSortedVars(methodBefore.name)
    val forAll = Forall(allArgs.head, allArgs.tail, implication)

    val prog =List(Assert(forAll), CheckSat())

    Right(prog.map(RecursivePrinter.toString).mkString)
  }

  def getMethodsInfo(methods: Map[EONamedBnd, MethodInfoForAnalysis]): EitherNel[String, Map[EONamedBnd, LMethod]] =
    methods.toList.foldLeft[EitherNel[String, Map[EONamedBnd, LMethod]]](Right(Map())) {
      case (acc, (key, value)) =>
        for {
          acc <- acc
          newVal <- processMethod(value, key.name.name, acc)
        } yield acc.updated(key, newVal)
    }

  def checkMethods
  (infoBefore: AnalysisInfo, infoAfter: AnalysisInfo): EitherNel[String, List[String]] = {
    val methodPairs = infoBefore.indirectMethods.alignWith(infoAfter.indirectMethods)(_.onlyBoth.get)

    methodPairs.toList.traverse {
      case (name, (before, after)) =>
        infoAfter.allMethods.foreach(m => println(m._2.body.toEOPretty))
        for {
          methodsBefore <- getMethodsInfo(infoBefore.allMethods)
          methodsAfter <- getMethodsInfo(infoAfter.allMethods)

          res1 <- processMethod(before, name.name.name, methodsBefore)
          res2 <- processMethod(after, name.name.name, methodsAfter)
          res <- checkImplication(res1, res2)
        } yield res
    }
  }

  def processObjectTree
  (objs: Map[EONamedBnd, ObjectTree[(AnalysisInfo, AnalysisInfo)]]): EitherNel[String, List[String]] = {
    objs.toList.flatTraverse {
      case (_, tree) =>
        for {
          currentRes <- checkMethods _ tupled tree.info
          recurseRes <- processObjectTree(tree.children)
        } yield currentRes ++ recurseRes
    }
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |[] > a
        |  [self x] > f
        |    x.sub 5 > y1
        |    seq > @
        |      assert (0.less y1)
        |      x
        |  [self y] > g
        |    self.f self y >  @
        |  [self z] > h
        |    z > @
        |[] > b
        |  a > @
        |  [self y] > f
        |    y > @
        |  [self z] > h
        |    self.g self z > @
        |""".stripMargin
//      """
//        |10 > seq
//        |22 > assert
//        |
//        |[] > base
//        |  [self x] > f
//        |    seq > @
//        |      assert (x.less 9)
//        |      x.add 1
//        |
//        |  [self b] > g
//        |    self.f self (b.add 1) > tmp
//        |    assert (tmp.less 10) > @
//        |
//        |[] > derived
//        |  base > @
//        |  [self x] > f
//        |    seq > @
//        |      assert (x.greater 5)
//        |      x.sub 1
//        |""".stripMargin

    Parser
      .parse(code)
      .flatMap(zipMethodsWithTheirInlinedVersionsFromParent)
      .map(tree =>
        processObjectTree(tree)
          .leftMap(println)
          .foreach(_.map(formula => {
            println(formula)
            SimpleAPI.withProver(dumpSMT = true)(p => {
              p.execSMTLIB(new StringReader(formula))
              println(p.partialModel)
            })
          }
          ))

      )
      .leftMap(println)
      .merge

  }
}
