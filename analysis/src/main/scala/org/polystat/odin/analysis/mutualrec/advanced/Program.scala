package org.polystat.odin.analysis.mutualrec.advanced

import cats.data.{NonEmptyList => Nel}
import org.polystat.odin.analysis.ObjectName
import org.polystat.odin.analysis.mutualrec.advanced.CallGraph._

object Program {
  type Program = List[Object]

  case class MethodName(whereDefined: ObjectName, name: String) {
    def show: String = s"${whereDefined.show}.$name"
  }

  case class ParentInfo(
    name: ObjectName,
    callGraph: CallGraph,
    parent: Option[ParentInfo],
  )

  case class Object(
    name: ObjectName, // full object name
    parent: Option[ParentInfo], // the object extended by this object
    nestedObjs: List[Object], // the objects inside this object
    callGraph: CallGraph, // the call graph for methods of this object
  ) {

    def extended(
      name: ObjectName, // name of the extending object
      cg: CallGraph, // call graph containing method (re-)definitions
      nestedObjs: List[Object],
    ): Object =
      Object(
        name = name,
        parent = Some(ParentInfo(this.name, this.callGraph, this.parent)),
        nestedObjs = nestedObjs,
        callGraph = this.callGraph.extendWith(cg)
      )

    def toEO: String = {
      def renderMethod(depth: Int)(cg: CallGraphEntry): String = {
        val spaces = "  " * depth
        cg match {
          case (name, calls) =>
            val renderedCalls = calls.toList match {
              case Nil => "self > @"
              case call :: Nil => s"self.${call.name} self > @"
              case calls =>
                val spaces2 = " " * (depth + 2)
                val spaces1 = " " * (depth + 1)
                val seq = calls
                  .map(call => s"${spaces2}self.${call.name} self")
                  .mkString("\n")
                s"${spaces1}seq > @\n$seq"
            }
            s"""$spaces[self] > ${name.name}
               |  $spaces$renderedCalls""".stripMargin
        }
      }

      def renderObject(obj: Object, depth: Int): String = {
        val spaces = "  " * (depth + 1)
        val locators = List.fill(depth + 1)('^').mkString(".")
        val parent = obj
          .parent
          .fold("")(parent => s"$spaces$locators.${parent.name.show} > @\n")

        val methods = obj
          .callGraph
          .filter { case (method, _) =>
            method.whereDefined == obj.name
          }
          .map(renderMethod(depth + 1))
          .mkString("\n") +
          (if (obj.callGraph.isEmpty && obj.nestedObjs.isEmpty) "" else "\n")

        val nestedObjs =
          if (obj.nestedObjs.isEmpty)
            ""
          else
            spaces + obj
              .nestedObjs
              .map(renderObject(_, depth + 1))
              .mkString("\n" + spaces)
        val header = s"[] > ${obj.name.name}\n"
        header + parent + methods + nestedObjs
      }

      renderObject(this, 0)
    }

    def toCPP: String = {
      def renderMethod(depth: Int)(cg: CallGraphEntry): String = {
        val spaces = "  " * depth
        cg match {
          case (name, calls) =>
            s"${spaces}virtual void ${name.name}(){${calls
                .map(call => s"${call.name}();")
                .mkString(s"\n  $spaces")}};"
        }
      }

      def helper(obj: Object, depth: Int): String = {
        val spaces = "  " * (depth + 1)
        val class_def = s"""class ${obj.name.name.toUpperCase()} ${obj
            .parent
            .fold("")(ext => s": public ${ext.name.name.toUpperCase()}")}"""
        val class_methods =
          "  " + obj
            .callGraph
            .filter { case (method, _) =>
              method.whereDefined.name == obj.name.name
            }
            .map(renderMethod(depth + 1))
            .mkString("\n  ")
        val nested_classes =
          obj
            .nestedObjs
            .map(helper(_, depth + 1))
            .mkString("\n  " + spaces)
        val nested_block =
          if (nested_classes.nonEmpty) spaces + "  " + nested_classes else ""
        val bracket_balance =
          if (depth == 0) "" else spaces
        s"""$class_def{
           |${spaces}public:
           |$class_methods${if (nested_classes.nonEmpty) "\n" else ""}
           |$nested_block$bracket_balance};\n""".stripMargin
      }

      helper(this, 0)
    }

    def toJava: String = {
      def renderMethod(depth: Int)(cg: CallGraphEntry): String = {
        val spaces = "  " * depth
        cg match {
          case (name, calls) =>
            s"${spaces}public void ${name.name}(){${calls
                .map(call => s"${call.name}();")
                .mkString(s"\n  $spaces")}};"
        }
      }

      def helper(obj: Object, depth: Int): String = {

        val spaces = "  " * (depth + 1)
        val class_def = s"""static class ${obj.name.name.toUpperCase()} ${obj
            .parent
            .fold("")(ext =>
              s" extends ${ext.name.names.map(_.toUpperCase).toList.mkString(".")}"
            )}"""
        val class_methods =
          "  " + obj
            .callGraph
            .filter { case (method, _) =>
              method.whereDefined.name == obj.name.name
            }
            .map(renderMethod(depth + 1))
            .mkString("\n  ")
        val nested_classes =
          obj
            .nestedObjs
            .map(helper(_, depth + 1))
            .mkString("\n  " + spaces)
        val nested_block =
          if (nested_classes.nonEmpty) spaces + "  " + nested_classes else ""
        val bracket_balance =
          if (depth == 0) "" else spaces
        s"""$class_def{
           |$class_methods${if (nested_classes.nonEmpty) "\n" else ""}
           |$nested_block$bracket_balance};\n""".stripMargin
      }

      helper(this, 0)
    }

    def toPython: String = {
      def renderMethod(depth: Int)(cg: CallGraphEntry): String = {
        val spaces = "  " * depth
        cg match {
          case (name, calls) =>
            s"""${spaces}def ${name.name}():
               |$spaces    pass
               |$spaces    ${calls
                .map(call => s"self.${call.name}()")
                .mkString(s"\n  $spaces")}
               |""".stripMargin
        }
      }

      def helper(obj: Object, depth: Int): String = {

        val spaces = "  " * (depth + 1)
        val class_def =
          s"""class ${obj.name.name.toUpperCase()}${obj
              .parent
              .fold("")(ext =>
                s"(${ext.name.names.map(_.toUpperCase).toList.mkString(".")})"
              )}:
             |$spaces  pass
            """.stripMargin
        val class_methods =
          "  " + obj
            .callGraph
            .filter { case (method, _) =>
              method.whereDefined.name == obj.name.name
            }
            .map(renderMethod(depth + 1))
            .mkString("\n  ")
        val nested_classes =
          obj
            .nestedObjs
            .map(helper(_, depth + 1))
            .mkString("\n  " + spaces)
        val nested_block =
          if (nested_classes.nonEmpty) spaces + "  " + nested_classes else ""
        val bracket_balance =
          if (depth == 0) "" else spaces
        s"""$class_def
           |$class_methods${if (nested_classes.nonEmpty) "\n" else ""}
           |$nested_block$bracket_balance\n""".stripMargin
      }

      helper(this, 0)
    }

  }

  implicit final class MethodNameOps(obj: String) {

    def %(method: String): MethodName =
      MethodName(ObjectName(Nel.one(obj)), method)

  }

  implicit final class ProgramOps(objs: Program) {

    def replaceObj(targetName: ObjectName, processObj: Object => Object): Program = {
      objs.map {
        case obj@Object(objName, _, _, _) if objName == targetName => processObj(obj)
        case obj@Object(_, Some(ParentInfo(name, _, _)), _, _) if name == targetName => processObj(obj)
        case other => other
      }
    }

    def containsObjectWithName(s: String): Boolean = {
      objs.exists(obj => obj.name.name == s)
    }

    def findCycles: List[CallChain] =
      objs
        .flatMap(obj => obj.callGraph.findCycles ++ obj.nestedObjs.findCycles)
        .distinct

    def findMultiObjectCycles: List[CallChain] =
      objs
        .flatMap(obj =>
          obj.callGraph.findMultiObjectCycles ++
            obj.nestedObjs.findMultiObjectCycles
        )
        .distinct

    def findMultiObjectCyclesWithObject: List[(ObjectName, CallChain)] = {
      objs
        .flatMap(obj =>
          obj.callGraph.findMultiObjectCycles.map((obj.name, _)) ++
            obj.nestedObjs.findMultiObjectCyclesWithObject
        )
    }

    def toCPP: String = objs.map(_.toCPP).mkString("\n")
    def toEO: String = objs.map(_.toEO).mkString("\n")
    def toJava: String = objs.map(_.toJava).mkString("\n")
    def toPython: String = objs.map(_.toPython).mkString("\n")

  }

}
