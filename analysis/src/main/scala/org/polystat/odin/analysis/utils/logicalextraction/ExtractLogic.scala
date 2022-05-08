package org.polystat.odin.analysis.utils.logicalextraction

import ap.SimpleAPI
import ap.SimpleAPI.FunctionalityMode
import cats.data.{EitherNel, NonEmptyList => Nel}
import cats.syntax.either._
import cats.syntax.traverse._
import higherkindness.droste.data.Fix
import org.polystat.odin.analysis.utils.logicalextraction.SMTUtils.Info
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import smtlib.printer.RecursivePrinter
import smtlib.theories.Core._
import smtlib.theories.Ints._
import smtlib.trees.Commands._
import smtlib.trees.Terms._

import java.io.StringReader


object ExtractLogic {

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
            SimpleIdentifier(SMTUtils.nameToSSymbol(List(name.name.name), depth))
          ),
          value
        )
    }
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
              SMTUtils.nameToSSymbol(List(name.name.name), depth),
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
            Right(SMTUtils.simpleAppToInfo(List(name), depth.drop(locator.toInt + 1)))
          case EODot(src, name) =>
            dotToSimpleAppsWithLocator(src, List(name)).map {
              case (locator, names) =>
                SMTUtils.simpleAppToInfo(names, depth.drop(locator.toInt + 1))
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

                      // Todo handle empty infos
                      //                    if (infos.nonEmpty)
                      Right(
                        Info(
                          List.empty,
                          List.empty,
                          FunctionApplication(
                            SMTUtils.mkValueFunIdent(
                              methodName,
                              depth.drop(locator.toInt + 1)
                            ),
                            infos.map(arg => arg.value)
                          ),
                          FunctionApplication(
                            SMTUtils.mkPropertiesFunIdent(
                              methodName,
                              depth.drop(locator.toInt + 1)
                            ),
                            infos.map(arg => arg.value)
                          )
                        )
                      )
                      //                    else
                      //                      Right(
                      //                        Info(
                      //                          List.empty,
                      //                          List.empty,
                      //                          mkValueFunIdent(
                      //                            methodName,
                      //                            depth.drop(locator.toInt + 1)
                      //                          ),
                      //                          mkPropertiesFunIdent(
                      //                            methodName,
                      //                            depth.drop(locator.toInt + 1)
                      //                          )
                      //                        )
                      //                      )
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
                case ("seq", arg :: Nil) =>
                  Right(
                    Info(
                      List.empty,
                      arg.exists,
                      arg.value,
                      arg.properties
                    )
                  )
                case ("seq", args) =>
                  Right(
                    Info(
                      List.empty,
                      args.last.exists,
                      args.last.value,
                      And(args.map(x => x.properties))
                    )
                  )
                // Todo if possible check that assert content is a boolean
                // assert (3.div 2) causes problems with type correspondance
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
                  Right(
                    Info(
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

  def checkImplication(
    methodName: String,
    before: Info,
    methodsBefore: Map[EONamedBnd, Info],
    after: Info,
    methodsAfter: Map[EONamedBnd, Info],
    resultMsgGenerator: String => String,
    beforeTag: String = "before",
    afterTag: String = "after"
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
        val defsBefore = methodsBefore.toList.flatMap { case (name, info) =>
          SMTUtils.mkFunDefs(beforeTag, name, info)
        }
        val defsAfter = methodsAfter.toList.flatMap { case (name, info) =>
          SMTUtils.mkFunDefs(afterTag, name, info)
        }
        val allDefs = defsBefore ++ defsAfter
        val callGraph = allDefs
          .map(func => (func, SMTUtils.extractMethodDependencies(func.body)))
          .flatMap { case (caller, vals) =>
            vals.map(callee => (caller, allDefs.find(_.name.name == callee)))
          }
          .collect { case (name, Some(value)) => (name, value) }
        val (properDefs, badDefs) = SMTUtils.tsort(callGraph)

        val cleansedDefs = badDefs
          .map { case (func, bads) =>
            SMTUtils.removeProblematicCalls(func, bads.map(_.name))
          }
        val orderedDefs = properDefs
          .toList
          .reverse
        val prog =
          (cleansedDefs ++ orderedDefs).map(DefineFun) ++ List(Assert(impl))

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
                  Some(resultMsgGenerator(methodName))
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
//        }

      // Todo support other cases
      // case (Nil, y :: ys ) => Left(Nel.one("Methods with no arguments are not
      // supported"))
      // case (x :: xs, Nil) => Left(Nel.one("Methods with no arguments are not
      // supported"))
      // case (Nil, Nil) => Left(Nel.one("Methods with no arguments are not
      // supported"))
      case (_, _) =>
        Left(Nel.one("Methods with no arguments are not supported"))
    }
  }

}
