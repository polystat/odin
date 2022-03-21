package org.polystat.odin.analysis.logicalexprs

import ap.SimpleAPI
import ap.SimpleAPI.FunctionalityMode
// import ap.util.UnionMap

// import scala.collection.immutable.{AbstractMap, SeqMap, SortedMap}
//import ap.proof.SimpleSimplifier
import cats.data.{EitherNel, NonEmptyList => Nel}
import cats.syntax.align._
import cats.syntax.either._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.inlining.Inliner.{
  zipMethodsWithTheirInlinedVersionsFromParent,
  AnalysisInfo
}
import org.polystat.odin.analysis.inlining.{MethodInfoForAnalysis, ObjectTree}
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.polystat.odin.parser.eo.Parser
import smtlib.printer.RecursivePrinter
import smtlib.theories.Ints._
import smtlib.trees.Commands._
import smtlib.trees.Terms._
// import org.polystat.odin.backend.eolang.ToEO.instances._
// import org.polystat.odin.backend.eolang.ToEO.ops._
import smtlib.theories.Core._

import java.io.StringReader
import scala.annotation.unused

object ExtractLogic {

  //  type ObjInfo = ObjectInfo[ParentInfo[MethodInfo, ObjectInfo], MethodInfo]

  // def extractAllMethodProperties(obj: ObjInfo): Map[MethodInfo, LMethod] =
  // ???

  val supportedOps: Map[String, (LExpr, LExpr) => LExpr] = Map(
    "div" -> LDiv.apply,
    "add" -> LAdd.apply,
    "sub" -> LSub.apply,
    "less" -> LLess.apply,
    "greater" -> LGreater.apply,
    "eq" -> LEq.apply
  )

  final case class Info(
    forall: List[SortedVar],
    exists: List[SortedVar],
    value: Term,
    properties: Term
  ) {}

  def nameToSSymbol(names: List[String], depth: List[String]): SSymbol = {
    val name = (depth.reverse ++ names).mkString("-")
    SSymbol(s"var-$name")
  }

  def simpleAppToInfo(names: List[String], depth: List[String]): Info = {
    Info(
      List.empty,
      List.empty,
      QualifiedIdentifier(SimpleIdentifier(nameToSSymbol(names, depth))),
      True()
    )
  }

  @annotation.tailrec
  def dotToSimpleAppsWithLocator(
    src: EOExprOnly,
    lastNames: List[String]
  ): EitherNel[String, (BigInt, List[String])] = {
    Fix.un(src) match {
      case EOObj(_, _, _) => Left(Nel.one("Cannot analyze [someObject].attr"))
      case app: EOApp[_] => app match {
          case EOSimpleApp(name) =>
            Left(Nel.one(s"Encountered unqualified attribute $name"))
          case EOSimpleAppWithLocator(name, locator) =>
            Right((locator, lastNames.prepended(name)))
          case EODot(src, name) =>
            dotToSimpleAppsWithLocator(src, lastNames.prepended(name))
          case EOCopy(_, _) =>
            Left(Nel.one("Cannot analyze dot of app:  (t1 t2).a"))
        }
      case _: EOData[_] =>
        Left(Nel.one("Cannot analyze arbitrary attributes of data"))
    }
  }

  def mkEqualsBndAttr(
    name: EONamedBnd,
    depth: List[String],
    value: Term
  ): Term = {
    value match {
      case QualifiedIdentifier(SimpleIdentifier(SSymbol("no-value")), _) =>
        True()
      case _ =>
        Equals(
          QualifiedIdentifier(
            SimpleIdentifier(nameToSSymbol(List(name.name.name), depth))
          ),
          value
        )
    }
  }

  def mkValueFunSSymbol(name: String, depth: List[String]): SSymbol = {
    val qualifiedName = (depth.reverse ++ List(name)).mkString("-")
    SSymbol(s"value-of-$qualifiedName")
  }

  def mkValueFunIdent(
    name: String,
    depth: List[String]
  ): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(mkValueFunSSymbol(name, depth)))
  }

  def mkPropertiesFunSSymbol(name: String, depth: List[String]): SSymbol = {
    val qualifiedName = (depth.reverse ++ List(name)).mkString("-")
    SSymbol(s"properties-of-$qualifiedName")
  }

  def mkPropertiesFunIdent(
    name: String,
    depth: List[String]
  ): QualifiedIdentifier = {
    QualifiedIdentifier(SimpleIdentifier(mkPropertiesFunSSymbol(name, depth)))
  }

  def extractInfo(
    depth: List[String],
    expr: EOExprOnly,
    availableMethods: Set[EONamedBnd]
  ): EitherNel[String, Info] = {
    Fix.un(expr) match {
      case EOObj(Vector(), None, bndAttrs) =>
        val infos = bndAttrs.traverse { case EOBndExpr(bndName, expr) =>
          extractInfo(bndName.name.name :: depth, expr, availableMethods).map(
            info => (bndName, info)
          )
        }
        infos.map(infos => {
          val localInfos = infos.filter {
            case (EODecoration, _) => false
            case _ => true
          }
          val newExists = localInfos.toList.flatMap { case (name, info) =>
            SortedVar(
              nameToSSymbol(List(name.name.name), depth),
              IntSort()
            ) :: info.exists
          }
          val newProperties = localInfos.toList match {
            case _ :: _ :: _ => And(localInfos.map { case (name, info) =>
                And(info.properties, mkEqualsBndAttr(name, depth, info.value))
              })
            case (name, info) :: Nil =>
              And(info.properties, mkEqualsBndAttr(name, depth, info.value))
            case Nil => True()
          }
          infos.toMap.get(EODecoration) match {
            case Some(resultInfo) =>
              Info(List.empty, newExists, resultInfo.value, newProperties)
            case None => Info(
                List.empty,
                newExists,
                QualifiedIdentifier(SimpleIdentifier(SSymbol("no-value"))),
                newProperties
              )
          }
        })
      case EOObj(_, _, _) =>
        Left(Nel.one("object with void attributes are not supported yet!")) /*
         * FIXME */
      case app: EOApp[_] => app match {
          case EOSimpleApp(name) =>
            Left(Nel.one(s"Encountered unqualified attribute $name"))
          case EOSimpleAppWithLocator(name, locator) =>
            Right(simpleAppToInfo(List(name), depth.drop(locator.toInt + 1)))
          case EODot(src, name) =>
            dotToSimpleAppsWithLocator(src, List(name)).map {
              case (locator, names) =>
                simpleAppToInfo(names, depth.drop(locator.toInt + 1))
            }
          case EOCopy(
                 Fix(
                   EODot(
                     Fix(EOSimpleAppWithLocator("self", locator)),
                     methodName
                   )
                 ),
                 args
               ) =>
            args.value.toList.map(x => Fix.un(x.expr)) match {
              case EOSimpleAppWithLocator("self", locator2) :: moreArgs
                   if locator == locator2 =>
                moreArgs
                  .traverse(expr =>
                    extractInfo(depth, Fix(expr), availableMethods)
                  )
                  .flatMap(infos =>
                    if (
                      availableMethods
                        .contains(EOAnyNameBnd(LazyName(methodName)))
                    ) {

                      if (infos.nonEmpty)
                        Right(
                          Info(
                            List.empty,
                            List.empty,
                            FunctionApplication(
                              mkValueFunIdent(
                                methodName,
                                depth.drop(locator.toInt + 1)
                              ),
                              infos.map(arg => arg.value)
                            ),
                            FunctionApplication(
                              mkPropertiesFunIdent(
                                methodName,
                                depth.drop(locator.toInt + 1)
                              ),
                              infos.map(arg => arg.properties)
                            )
                          )
                        )
                      else
                        Right(
                          Info(
                            List.empty,
                            List.empty,
                            mkValueFunIdent(
                              methodName,
                              depth.drop(locator.toInt + 1)
                            ),
                            mkPropertiesFunIdent(
                              methodName,
                              depth.drop(locator.toInt + 1)
                            )
                          )
                        )
                    } else
                      Left(Nel.one(s"Unknown method $methodName"))
                  )
              case _ => Left(Nel.one(s"Unsupported EOCopy with self: $app"))
            }
          case EOCopy(Fix(EOSimpleAppWithLocator(name, _)), args) => for { /*
               * FIXME: check locators */
              infoArgs <- args
                .value
                .traverse(arg => extractInfo(depth, arg.expr, availableMethods))
              result <- (name, infoArgs.toList) match {
                case ("seq", Nil) =>
                  Left(Nel.one("seq is expecting at least one term"))
                case ("seq", args) =>
                  Right(
                    Info(
                      List.empty,
                      args.last.exists,
                      args.last.value,
                      And(args.map(x => x.properties))
                    )
                  )
                case ("assert", arg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      arg.value,
                      And(arg.properties, arg.value)
                    )
                  )
                case _ => Left(
                    Nel.one(
                      s"Unsupported ${infoArgs.length}-ary primitive $name"
                    )
                  )
              }
            } yield result
          case EOCopy(Fix(EODot(src, attr)), args) => for {
              infoSrc <- extractInfo(depth, src, availableMethods)
              infoArgs <- args
                .value
                .traverse(arg => extractInfo(depth, arg.expr, availableMethods))
              result <- (attr, infoArgs.toList) match {
                case ("add", infoArg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      Add(infoSrc.value, infoArg.value),
                      And(infoSrc.properties, infoArg.properties)
                    )
                  )
                case ("div", infoArg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      Div(infoSrc.value, infoArg.value),
                      And(
                        infoSrc.properties,
                        infoArg.properties,
                        Not(Equals(infoArg.value, SNumeral(0)))
                      )
                    )
                  )
                case ("mul", infoArg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      Mul(infoSrc.value, infoArg.value),
                      And(infoSrc.properties, infoArg.properties)
                    )
                  )
                case ("sub", infoArg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      Sub(infoSrc.value, infoArg.value),
                      And(infoSrc.properties, infoArg.properties)
                    )
                  )
                case ("less", infoArg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      LessThan(infoSrc.value, infoArg.value),
                      And(infoSrc.properties, infoArg.properties)
                    )
                  )
                case ("greater", infoArg :: Nil) =>
                  Right(Info(
                      List.empty,
                      List.empty,
                      GreaterThan(infoSrc.value, infoArg.value),
                      And(infoSrc.properties, infoArg.properties)
                    )
                  )
                case ("if", ifTrue :: ifFalse :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      List.empty,
                      ITE(infoSrc.value, ifTrue.value, ifFalse.value),
                      And(
                        infoSrc.properties,
                        Or(
                          And(infoSrc.value, ifTrue.properties),
                          And(Not(infoSrc.value), ifFalse.properties)
                        )
                      )
                    )
                  )
                case _ => Left(
                    Nel.one(
                       s"Unsupported ${infoArgs.length}-ary primitive .$attr"
                    )
                  )
              }
            } yield result
          case _ => Left(Nel.one(s"Some EOCopy is not supported yet: $app"))
        }
      case EOIntData(n) =>
        Right(Info(List.empty, List.empty, SNumeral(n), True()))
      case _ => Left(Nel.one(s"Some case is not checked: $expr")) // FIXME
    }
  }

  def mkIntVar(name: String, depth: List[String]): SortedVar = {
    SortedVar(nameToSSymbol(List(name), depth), IntSort())
  }

  def processMethod2(
    tag: String,
    method: MethodInfoForAnalysis,
    name: String,
    availableMethods: Set[EONamedBnd]
  ): EitherNel[String, Info] = {
    val body = method.body
    val depth = List(tag)

    body.bndAttrs.collectFirst { case EOBndExpr(EODecoration, expr) =>
      expr
    } match {
      case Some(_) => {
        // val arguments = body.freeAttrs.tail // FIXME: we are assuming first
        // argument is self (need to check)
        val infos = body.bndAttrs.traverse {
          case EOBndExpr(bndName, expr) => {
            extractInfo(bndName.name.name :: depth, expr, availableMethods)
              .map(info => (bndName, info))
          }
        }
        infos.flatMap(infos =>
          infos.toMap.get(EODecoration) match {
            case Some(resultInfo) => Right {
                val localInfos = infos.filter {
                  case (EODecoration, _) => false
                  case _ => true
                }
                val newExists = localInfos.toList.flatMap { case (name, info) =>
                  SortedVar(
                    nameToSSymbol(List(name.name.name), depth),
                    IntSort()
                  ) :: info.exists
                }
                val newProperties = localInfos.toList match {
                  case _ :: _ :: _ => And(localInfos.map { case (name, info) =>
                      And(
                        info.properties,
                        mkEqualsBndAttr(name, depth, info.value)
                      )
                    })
                  case (name, info) :: Nil =>
                    And(
                      info.properties,
                      mkEqualsBndAttr(name, depth, info.value)
                    )
                  case Nil => True()
                }
                val params = body
                  .freeAttrs
                  .tail
                  .toList
                  .map(name => mkIntVar(name.name, depth))
                newExists match {
                  // FIXME: add Let for value (note, you need to store bindings,
                  // not only names in "exists")
                  case x :: xs => Info(
                      params,
                      List.empty,
                      resultInfo.value,
                      Exists(x, xs, And(resultInfo.properties, newProperties))
                    )
                  case Nil => Info(
                      params,
                      List.empty,
                      resultInfo.value,
                      And(resultInfo.properties, newProperties)
                    )
                }
              }
            case None => Left(Nel.one("Impossible happened!"))
          }
        )
      }
      case None =>
        Left(Nel.one(s"Method $name does not have attached @ attribute"))
    }

  }

  def mkFunDecls(tag: String, name: EONamedBnd, info: Info): List[Command] = {
    val valueDef = DefineFun(
      FunDef(
        mkValueFunSSymbol(name.name.name, List(tag)),
        info.forall,
        IntSort(),
        info.value
      )
    )
    val propertiesDef = DefineFun(
      FunDef(
        mkPropertiesFunSSymbol(name.name.name, List(tag)),
        info.forall,
        BoolSort(),
        info.properties
      )
    )
    List(valueDef, propertiesDef)
  }

  def checkImplication2(
    methodName: String,
    before: Info,
    methodsBefore: Map[EONamedBnd, Info],
    after: Info,
    methodsAfter: Map[EONamedBnd, Info]
  ): EitherNel[String, Option[String]] = {
    (before.forall, after.forall) match {
      case (x :: xs, y :: ys) =>
        val impl = Forall(
          x,
          xs,
          Exists(
            y,
            ys,
            And(
              And(True() :: before.forall.zip(after.forall).map { case (x, y) =>
                Equals(
                  QualifiedIdentifier(SimpleIdentifier(SSymbol(x.name.name))),
                  QualifiedIdentifier(SimpleIdentifier(SSymbol(y.name.name)))
                )
              }),
              Implies(before.properties, after.properties)
            )
          )
        )
        val declsBefore = methodsBefore.toList.flatMap { case (name, info) =>
          mkFunDecls("before", name, info)
        }
        val declsAfter = methodsAfter.toList.flatMap { case (name, info) =>
          mkFunDecls("after", name, info)
        }
        val prog = declsBefore ++ declsAfter ++ List(Assert(impl))
        val formula = prog.map(RecursivePrinter.toString).mkString

        //        println(formula)

        util
          .Try(SimpleAPI.withProver(p => {
            val (assertions, functions, constants, predicates) =
              p.extractSMTLIBAssertionsSymbols(
                new StringReader(formula),
                fullyInline = true
              )
            assertions.foreach(p.addAssertion)
            functions
              .keySet
              .foreach(f => p.addFunction(f, FunctionalityMode.NoUnification))
            constants.keySet.foreach(p.addConstantRaw)
            predicates.keySet.foreach(p.addRelation)
            p.checkSat(true)
            p.getStatus(true) match {
              case ap.SimpleAPI.ProverStatus.Sat => Right(None)
              case ap.SimpleAPI.ProverStatus.Unsat => Right(
                  Some(s"Method $methodName is not referentially transparent")
                )
              case err => Left(Nel.one(s"SMT solver failed with error: $err"))
            }
          }))
          .toEither
          .leftMap(t =>
            Nel.one(
              s"SMT failed to parse the generated program with error: ${t.getMessage}"
            )
          )
          .flatten

      // case (Nil, y :: ys ) => Left(Nel.one("Methods with no arguments are not
      // supported"))
      // case (x :: xs, Nil) => Left(Nel.one("Methods with no arguments are not
      // supported"))
      // case (Nil, Nil) => Left(Nel.one("Methods with no arguments are not
      // supported"))
      case (Nil, Nil) =>
        val impl = Implies(before.properties, after.properties)
        val declsBefore = methodsBefore.toList.flatMap { case (name, info) =>
          mkFunDecls("before", name, info)
        }
        val declsAfter = methodsAfter.toList.flatMap { case (name, info) =>
          mkFunDecls("after", name, info)
        }
        val prog = declsBefore ++ declsAfter ++ List(Assert(impl))
        val formula = prog.map(RecursivePrinter.toString).mkString

        util
          .Try(SimpleAPI.withProver(p => {
            val (assertions, functions, constants, predicates) =
              p.extractSMTLIBAssertionsSymbols(
                new StringReader(formula),
                fullyInline = true
              )
            assertions.foreach(p.addAssertion)
            functions
              .keySet
              .foreach(f => p.addFunction(f, FunctionalityMode.NoUnification))
            constants.keySet.foreach(p.addConstantRaw)
            predicates.keySet.foreach(p.addRelation)
            p.checkSat(true)
            p.getStatus(true) match {
              case ap.SimpleAPI.ProverStatus.Sat => Right(None)
              case ap.SimpleAPI.ProverStatus.Unsat => Right(
                  Some(s"Method $methodName is not referentially transparent")
                )
              case err => Left(Nel.one(s"SMT solver failed with error: $err"))
            }
          }))
          .toEither
          .leftMap(t =>
            Nel.one(
              s"SMT failed to parse the generated program with error: ${t.getMessage}"
            )
          )
          .flatten

      case (_, _) =>
        Left(Nel.one("Methods with no arguments are not supported"))

      case _ => Left(Nel.one("Methods with no arguments are not supported"))
    }
  }

  def processMethod(
    method: MethodInfoForAnalysis,
    name: String,
    availableMethods: Map[EONamedBnd, LMethod]
  ): EitherNel[String, LMethod] = {

    def extractLogic(
      @unused depth: BigInt,
      @unused lastBnd: Option[EOBndExpr[EOExprOnly]],
      accPrefix: String = ""
    )(expr: EOExprOnly): EitherNel[String, LExpr] = Fix.un(expr) match {
      //      case EOObj(freeAttrs, varargAttr, bndAttrs) => ???

      case EODot(expr, name) =>
        extractLogic(depth, lastBnd, name + accPrefix)(expr)

      case obj @ EOObj(Vector(), _, bndAttrs) =>
        //        val phi = bndAttrs
        //          .collectFirst { case EOBndExpr(EODecoration, expr) => expr }
        //          .map(extractLogic(0, None))
        val nonPhi = bndAttrs.filter {
          case EOBndExpr(EODecoration, _) => false
          case _ => true
        }

        for {
          // phi <- phi.getOrElse(Left("The given method has no phi
          // attribute!"))
          attrs <- nonPhi.traverse(bnd => {
            val name = bnd.bndName.name.name
            val expr = extractLogic(method.depth, Some(bnd))(bnd.expr)

            expr.map(AttrBnd(_, name))
          })
          name <- lastBnd
            .toRight(Nel.one(s"No name found for $obj"))
            .map(_.bndName.name.name)
        } yield LObj(attrs.toList, None, name)

      case copy @ EOCopy(EOSimpleAppWithLocator("assert", _), args) =>
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

      case copy @ EOCopy(Fix(EODot(src, op)), args)
           if supportedOps.contains(op) =>
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

      case EOCopy(
             Fix(EODot(EOSimpleAppWithLocator("self", _), methodName)),
             args
           ) =>
        for {
          processedArgs <- args
            .value
            .toList
            // TODO properly add methodname
            .traverse(b => extractLogic(depth, lastBnd, accPrefix)(b.expr))
          expectedMethod <-
            availableMethods
              .get(EOAnyNameBnd(LazyName(methodName)))
              .toRight(
                Nel.one(s"Attempt to call non-existent method $methodName")
              )
        } yield LCall(expectedMethod, processedArgs)

      case EOSimpleAppWithLocator(name, _) =>
        Right(Attr(name + accPrefix))
      case EOIntData(int) => Right(LConst(int))
      //      case app: EOApp[_] => ???
      //      case data: EOData[_] => ???
      case expr => Left(Nel.one(s"Unsupported expression $expr"))
    }

    val body = method.body
    val phi = body
      .bndAttrs
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

  def checkImplication(
    @unused methodBefore: LMethod,
    @unused methodAfter: LMethod
  ): EitherNel[String, String] = {

    val props1 = methodBefore.properties("")
    //    val props2 = methodAfter.properties("")

    //    val implication = Assert(Equals(Implies(props1, props2), False()))
    //    val implication   = Implies(props1, props2)
    val implication = props1

    //        val a = Equals(props1, True())
    //        val b = Equals(props2, False())
    //        val c = Equals(And(a,b),False())
    //        val implication = Assert(c)

    val allArgs = methodAfter.argumentSortedVars(
      methodAfter.name
    ) ++ methodBefore.argumentSortedVars(methodBefore.name)
    val forAll = Forall(allArgs.head, allArgs.tail, implication)

    val prog = List(Assert(forAll), CheckSat())
    SimpleAPI.withProver(dumpSMT = true)(p => {

      println(p.partialModel)
    })

    Right(prog.map(RecursivePrinter.toString).mkString)
  }

  def getMethodsInfo(
    tag: String,
    methods: Map[EONamedBnd, MethodInfoForAnalysis]
  ): EitherNel[String, Map[EONamedBnd, Info]] = {
    val methodNames = methods.keySet
    methods
      .toList
      .foldLeft[EitherNel[String, Map[EONamedBnd, Info]]](Right(Map())) {
        case (acc, (key, value)) =>
          for {
            acc <- acc
            newVal <- processMethod2(tag, value, key.name.name, methodNames)
          } yield acc.updated(key, newVal)
      }
  }

  def checkMethods(
    infoBefore: AnalysisInfo,
    infoAfter: AnalysisInfo
  ): EitherNel[String, List[String]] = {
    val methodPairs = infoBefore
      .indirectMethods
      .alignWith(infoAfter.indirectMethods)(_.onlyBoth.get)

    methodPairs.toList.flatTraverse { case (name, (before, after)) =>
      val methodName = name.name.name
      // println("==================================================")
      // println(before.body.toEOPretty)
      // println("==================================================")
      // println(after.body.toEOPretty)
      // println("==================================================")
      for {
        methodsBefore <- getMethodsInfo("before", infoBefore.allMethods)
        methodsAfter <- getMethodsInfo("after", infoAfter.allMethods)

        res1 <-
          processMethod2("before", before, methodName, methodsBefore.keySet)
        res2 <- processMethod2("after", after, methodName, methodsAfter.keySet)
        res <-
          checkImplication2(methodName, res1, methodsBefore, res2, methodsAfter)
      } yield res.toList
    }
  }

  def processObjectTree(
    objs: Map[EONamedBnd, ObjectTree[(AnalysisInfo, AnalysisInfo)]]
  ): EitherNel[String, List[String]] = {
    objs.toList.flatTraverse { case (_, tree) =>
      for {
        currentRes <- checkMethods _ tupled tree.info
        recurseRes <- processObjectTree(tree.children)
      } yield currentRes ++ recurseRes
    }
  }

  def main(args: Array[String]): Unit = {
    val code =
      """
        |[] > test
        |  [] > base
        |    [self x] > f
        |      x.sub 5 > y1
        |      seq > @
        |        assert (0.less y1)
        |        x
        |    [self y] > g
        |      self.f self y >  @
        |    [self z] > h
        |      z > @
        |
        |  [] > derived
        |    base > @
        |    [self y] > f
        |      y > @
        |    [self z] > h
        |      self.g self z > @
        |      
        |   
        |  [] > a
        |    [] > new
        |      b.new > @
        |      [self x y] > func
        |        self.gonc self y x > @
        |  
        |  [] > b
        |    [] > new
        |      c.new > @
        |  [] > c
        |    [] > new
        |      [self y x] > gonc
        |        self.func self x y > @
        |      [self x y] > func
        |        self > @
        |  
        |""".stripMargin
//      """
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
      .map(tree => {
        processObjectTree(tree)
          .leftMap(println)
          .foreach(_.map(formula => {
            println(formula)
          }))
      })
      .leftMap(println)
      .merge

  }

}
