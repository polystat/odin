package eo.backend.eolang.abstractions

abstract class ToEOBnd[T, R, NR](implicit toEO: ToEO[T, R], eoRepr: EORepr[R, NR]) {
//  def withBindToName(f: T => R): T => NR => R = eoRepr.bindToName compose f
  def bndToEO: T => NR => R = eoRepr.bindToName compose toEO.toEO
}

object ToEOBnd {
  def apply[T, R, NR](implicit toEOBnd: ToEOBnd[T, R, NR]): ToEOBnd[T, R, NR] = toEOBnd

  object ops {
    def bndToEO[T, R, NR](src: T)(name: NR)(implicit toEOBnd: ToEOBnd[T, R, NR]): R =
      ToEOBnd[T, R, NR].bndToEO(src)(name)

    implicit class ToEOBndOps[T, R, NR](val src: T) extends AnyVal {
      def bndToEO(name: NR)(implicit toEOBnd: ToEOBnd[T, R, NR]): R =
        ToEOBnd[T, R, NR].bndToEO(src)(name)
    }
  }
}
