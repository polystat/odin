package eo.backend.eolang.instances

import eo.backend.eolang.abstractions.ToEO
import eo.core.ast.EOBnd

object Bindings {
  implicit val bndToEO: ToEO[EOBnd, Iterable[String]] =
    new ToEO[EOBnd, Iterable[String]] {
      // TODO: implement
      override def toEO(node: EOBnd): Iterable[String] = ???
    }
}
