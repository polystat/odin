// TODO: Unjustified assumption and Mutual Recursion analyses share a lot in common.
// TODO: Would be nice to abstract the common stuff into this file
//package org.polystat.odin
//
//import scala.annotation.tailrec
//import analysis.Methods._
//import analysis.MethodName._
//
//package analysis {
//
//  case class ObjectName(container: Option[ObjectName], name: String) {
//    def show: String = container.fold(name)(p => s"${p.show}.$name")
//  }
//
//  object Methods {
//
//    type Methods[M] = Map[MethodName, M]
//    type Method[M] = (MethodName, M)
//
//    implicit final class MethodsOps[M](self: Methods[M]) {
//
//      def containsMethodWithName(name: String): Boolean = {
//        self.keySet.map(_.name).contains(name)
//      }
//
//      def extendWith(other: Methods[M]): Methods[M] = {
//
//        @tailrec
//        def extendWithRec(
//          rem: List[Method[M]],
//          acc: Methods[M]
//        ): Methods[M] =
//          rem match {
//            case Nil => acc
//            case (newMethod, calls) :: tail =>
//              extendWithRec(
//                tail,
//                acc
//                  .find { case (oldMethod, _) =>
//                    oldMethod.name == newMethod.name
//                  }
//                  .fold[Methods[M]](acc.updated(newMethod, calls)) {
//                    case (oldMethod, _) => acc
//                        .removed(oldMethod)
//                        .updated(newMethod, calls)
//                  }
//              )
//          }
//
//        extendWithRec(other.toList, self)
//      }
//
//    }
//
//  }
//
//  object MethodName {
//
//    case class MethodName(whereDefined: ObjectName, name: String) {
//      def show: String = s"${whereDefined.show}.$name"
//    }
//
//    implicit final class MethodNameOps(obj: String) {
//
//      def %(method: String): MethodName =
//        MethodName(ObjectName(None, obj), method)
//
//    }
//
//  }
//
//  object types {
//
//    type NestedObjs[M] = Map[ObjectName, ObjectInfo[M]]
//    type Program[M] = Vector[ObjectInfo[M]]
//
//    trait ParentInfo[M] {
//      val name: ObjectName
//      val methods: Methods[M]
//      val parent: Option[ParentInfo[M]]
//    }
//
//    trait ObjectInfo[M] {
//      val name: ObjectName
//      val parent: Option[ParentInfo[M]]
//      val nestedObjs: NestedObjs[M]
//      val methods: Methods[M]
//    }
//
//  }
//
//}
