package eo.backend.eolang.abstractions

trait ToEO[T, R] {
  def toEO(node: T): R
}

object ToEO {
  def apply[T, R](implicit toEO: ToEO[T, R]): ToEO[T, R] = toEO

  object ops {
    def toEO[T, R](node: T)(implicit toEO: ToEO[T, R]): R = ToEO[T, R].toEO(node)

    implicit class ToEOOps[T, R](val node: T) extends AnyVal {
      def toEO(implicit toEO: ToEO[T, R]): R = ToEO[T, R]toEO(node)
    }
  }
}
