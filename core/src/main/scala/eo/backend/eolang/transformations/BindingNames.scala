package eo.backend.eolang.transformations

import eo.core.ast.{ BndName, ConstBnd, Constants, LazyBnd }

object BindingNames {
  def lazyBndNameToEO(lb: LazyBnd): String =
    lb.name

  def constBndNameToEO(cb: ConstBnd): String =
    s"${cb.name}${Constants.SYMBS.CONST_MOD}"

  def bndNameToEO: BndName => String = {
    case lb@LazyBnd(_) => lazyBndNameToEO(lb)
    case cb@ConstBnd(_) => constBndNameToEO(cb)
  }
}
