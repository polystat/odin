package eo.backend.eolang.abstractions

trait EORepr[R, NR] {
  def bindToName: R => NR => R
}

object EORepr {
  def apply[R, NR](implicit eoRepr: EORepr[R, NR]): EORepr[R, NR] = eoRepr

  object ops {
    def bindToName[R, NR](src: R)(name: NR)(implicit eoRepr: EORepr[R, NR]): R =
      EORepr[R, NR].bindToName(src)(name)

    implicit class EOReprOps[R, NR](val src: R) extends AnyVal {
      def bindToName(name: NR)(implicit eoRepr: EORepr[R, NR]): R =
        EORepr[R, NR].bindToName(src)(name)
    }
  }
}
