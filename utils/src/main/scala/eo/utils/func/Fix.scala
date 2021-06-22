package eo.utils.func

case class Fix[F[_]](unfix: F[Fix[F]])
