package org.polystat.odin.analysis.utils.j2eo

object Primitives {

  val all: Seq[String] = Seq(
//    """
//      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
//      |+package stdlib.primitives
//      |+alias stdlib.primitives.prim__num
//      |
//      |[] > prim__boolean
//      |  prim__num > super
//      |  super > @
//      |
//      |  [] > new
//      |    [] > this
//      |      memory > v
//      |      prim__num.new > super
//      |      super > @
//      |      "prim__boolean" > prim_name
//      |
//      |      [] > as-string
//      |        seq > @
//      |          if.
//      |            ^.v
//      |            "true"
//      |            "false"
//      |
//      |      [right] > write
//      |        seq > @
//      |          ^.v.write
//      |            right.v
//      |          prim__boolean.constructor_3
//      |            prim__boolean.new
//      |            ^
//      |
//      |      [right] > and
//      |        seq > @
//      |          prim__boolean.constructor_2
//      |            prim__boolean.new
//      |            ^.v.and
//      |              right.v
//      |
//      |      [right] > or
//      |        seq > @
//      |          prim__boolean.constructor_2
//      |            prim__boolean.new
//      |            ^.v.or
//      |              right.v
//      |
//      |      [] > not
//      |        seq > @
//      |          prim__boolean.constructor_2
//      |            prim__boolean.new
//      |            ^.v.not
//      |
//      |      [t_s f_s] > if
//      |        seq > @
//      |          if.
//      |            ^.v
//      |            t_s
//      |            f_s
//      |
//      |      [f] > while
//      |        seq > @
//      |          while.
//      |            ^.v
//      |            f
//      |
//      |    seq > @
//      |      this
//      |
//      |  [this] > constructor_1
//      |    seq > @
//      |      super.constructor_1
//      |        this
//      |
//      |  [this target] > constructor_2
//      |    seq > @
//      |      super.constructor_2
//      |        this
//      |        target
//      |
//      |  [this another_num] > constructor_3
//      |    seq > @
//      |      super.constructor_3
//      |        this
//      |        another_num
//      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__byte
      |  prim__num > super
      |  super > @
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__byte" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__byte.constructor_3
      |            prim__byte.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__byte.constructor_3
      |            prim__byte.new
      |            ^
      |
      |      [] > inc_post
      |        prim__byte.constructor_1 > old
      |          prim__byte.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__byte.constructor_1 > old
      |          prim__byte.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__byte.constructor_3
      |            prim__byte.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__byte.constructor_2
      |            prim__byte.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__byte.constructor_2
      |            prim__byte.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__byte.constructor_2
      |            prim__byte.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__byte.constructor_2
      |            prim__byte.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__byte.constructor_2
      |            prim__byte.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__char
      |  prim__num > super
      |  super > @
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__char" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__char.constructor_3
      |            prim__char.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__char.constructor_3
      |            prim__char.new
      |            ^
      |
      |      [] > inc_post
      |        prim__char.constructor_1 > old
      |          prim__char.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__char.constructor_1 > old
      |          prim__char.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__char.constructor_3
      |            prim__char.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__char.constructor_2
      |            prim__char.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__char.constructor_2
      |            prim__char.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__char.constructor_2
      |            prim__char.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__char.constructor_2
      |            prim__char.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__char.constructor_2
      |            prim__char.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__double
      |  prim__num > super
      |  super > @
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__double" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__double.constructor_3
      |            prim__double.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__double.constructor_3
      |            prim__double.new
      |            ^
      |
      |      [] > inc_post
      |        prim__double.constructor_1 > old
      |          prim__double.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__double.constructor_1 > old
      |          prim__double.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__double.constructor_3
      |            prim__double.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__double.constructor_2
      |            prim__double.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__double.constructor_2
      |            prim__double.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__double.constructor_2
      |            prim__double.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__double.constructor_2
      |            prim__double.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__double.constructor_2
      |            prim__double.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__float
      |  prim__num > super
      |  super > @
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__float" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__float.constructor_3
      |            prim__float.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__float.constructor_3
      |            prim__float.new
      |            ^
      |
      |      [] > inc_post
      |        prim__float.constructor_1 > old
      |          prim__float.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__float.constructor_1 > old
      |          prim__float.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__float.constructor_3
      |            prim__float.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__float.constructor_2
      |            prim__float.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__float.constructor_2
      |            prim__float.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__float.constructor_2
      |            prim__float.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__float.constructor_2
      |            prim__float.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__float.constructor_2
      |            prim__float.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__int
      |  prim__num > super
      |  super > @
      |  2147483647 > max_int
      |  -2147483648 > min_int
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__int" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__int.constructor_3
      |            prim__int.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__int.constructor_3
      |            prim__int.new
      |            ^
      |
      |      [] > inc_post
      |        prim__int.constructor_1 > old
      |          prim__int.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__int.constructor_1 > old
      |          prim__int.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__int.constructor_3
      |            prim__int.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__int.constructor_2
      |            prim__int.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__int.constructor_2
      |            prim__int.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__int.constructor_2
      |            prim__int.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__int.constructor_2
      |            prim__int.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__int.constructor_2
      |            prim__int.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [right] > from
      |    seq > @
      |      prim__int.constructor_2
      |        prim__int.new
      |        mod.
      |          this.integer_part
      |            right
      |          max_int
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
    """
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__boolean
      |
      |[] > prim__num
      |  [] > new
      |    []> this
      |      memory > v
      |    seq > @
      |      this
      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__short
      |  prim__num > super
      |  super > @
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__short" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__short.constructor_3
      |            prim__short.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__short.constructor_3
      |            prim__short.new
      |            ^
      |
      |      [] > inc_post
      |        prim__short.constructor_1 > old
      |          prim__short.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__short.constructor_1 > old
      |          prim__short.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__short.constructor_3
      |            prim__short.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__short.constructor_2
      |            prim__short.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__short.constructor_2
      |            prim__short.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__short.constructor_2
      |            prim__short.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__short.constructor_2
      |            prim__short.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__short.constructor_2
      |            prim__short.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
    """
      |# inc_pre, dec_pre, inc_post, dec_post, add, sub, mul, div are not precise operators
      |+package stdlib.primitives
      |+alias stdlib.primitives.prim__num
      |
      |[] > prim__long
      |  prim__num > super
      |  super > @
      |
      |  [] > new
      |    [] > this
      |      memory > v
      |      prim__num.new > super
      |      super > @
      |      "prim__long" > prim_name
      |
      |      [] > inc_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.add
      |              1
      |          prim__long.constructor_3
      |            prim__long.new
      |            ^
      |
      |      [] > dec_pre
      |        seq > @
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          prim__long.constructor_3
      |            prim__long.new
      |            ^
      |
      |      [] > inc_post
      |        prim__long.constructor_1 > old
      |          prim__long.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.add
      |              1
      |          old
      |
      |      [] > dec_post
      |        prim__long.constructor_1 > old
      |          prim__long.new
      |        seq > @
      |          old.v.write
      |            ^.v
      |          ^.v.write
      |            ^.v.sub
      |              1
      |          old
      |
      |      [right] > write
      |        seq > @
      |          ^.v.write
      |            right.v
      |          prim__long.constructor_3
      |            prim__long.new
      |            ^
      |
      |      [right] > add
      |        seq > @
      |          prim__long.constructor_2
      |            prim__long.new
      |            ^.v.add
      |              right.v
      |
      |      [right] > sub
      |        seq > @
      |          prim__long.constructor_2
      |            prim__long.new
      |            ^.v.sub
      |              right.v
      |
      |      [right] > mul
      |        seq > @
      |          prim__long.constructor_2
      |            prim__long.new
      |            ^.v.mul
      |              right.v
      |
      |      [right] > div
      |        seq > @
      |          prim__long.constructor_2
      |            prim__long.new
      |            ^.v.div
      |              right.v
      |
      |      [right] > mod
      |        seq > @
      |          prim__long.constructor_2
      |            prim__long.new
      |            ^.v.mod
      |              right.v
      |
      |    seq > @
      |      this
      |
      |  [this] > constructor_1
      |    seq > @
      |      super.constructor_1
      |        this
      |
      |  [this target] > constructor_2
      |    seq > @
      |      super.constructor_2
      |        this
      |        target
      |
      |  [this another_num] > constructor_3
      |    seq > @
      |      super.constructor_3
      |        this
      |        another_num
      |""".stripMargin,
  )

}
