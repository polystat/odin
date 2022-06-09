package org.polystat.odin.analysis.utils.j2eo

object Other {

  val all: Seq[String] = Seq(
    """
      |+package stdlib.lang
      |
      |[] > class__Object
      |  [] > new
      |    [] > this
      |      "class__Object" > className
      |      "OBJECT" > @
      |
      |      [this] > init
      |        0 > @
      |
      |      [this] > toString
      |        "OBJECT" > @
      |
      |    seq > @
      |      this
      |
      |  [self] > constructor
      |    seq > @
      |      this.init
      |        self
      |      self
      |""".stripMargin,
//    """
//      |+package stdlib.io
//      |+alias stdlib.lang.class__Object
//      |+alias org.eolang.io.stdout
//      |
//      |[] > class__PrintStream
//      |  class__Object > super
//      |  super > @
//      |
//      |  [] > new
//      |    [] > this
//      |      class__Object.new > super
//      |      super > @
//      |      "class__PrintStream" > className
//      |
//      |      [this] > init
//      |        0 > @
//      |
//      |      [this output] > println
//      |        seq > @
//      |          stdout
//      |            str.
//      |              output.toString
//      |                output
//      |          stdout
//      |            "\n"
//      |
//      |    seq > @
//      |      this
//      |
//      |  [self] > constructor
//      |    seq > @
//      |      this.init
//      |        self
//      |      self
//      |""".stripMargin,
//    """
//      |+package stdlib.lang
//      |+alias stdlib.lang.class__Object
//      |+alias org.eolang.txt.sprintf
//      |
//      |[] > class__String
//      |  class__Object > super
//      |  super > @
//      |
//      |  [] > new
//      |    [] > this
//      |      class__Object.new > super
//      |      super > @
//      |      "class__String" > className
//      |
//      |      memory > str
//      |
//      |      [this input_string] > init
//      |        seq > @
//      |          this.str.write
//      |            input_string
//      |
//      |      [this] > toString
//      |        seq > @
//      |          class__String.constructor_3
//      |            class__String.new
//      |            this
//      |
//      |      [right] > write
//      |        seq > @
//      |          ^.str.write
//      |            right.str
//      |          class__String.constructor_3
//      |            class__String.new
//      |            ^
//      |
//      |      [right] > add
//      |        seq > @
//      |          class__String.constructor_2
//      |            class__String.new
//      |            sprintf
//      |              "%s%s"
//      |              ^.str
//      |              str.
//      |                right.toString
//      |                  right
//      |
//      |    seq > @
//      |      this
//      |
//      |  [right] > valueOf
//      |    seq > @
//      |      class__String.constructor_2
//      |        class__String.new
//      |        right.as-string
//      |
//      |  [this] > constructor_1
//      |    seq > @
//      |      this
//      |
//      |  [this target] > constructor_2
//      |    seq > @
//      |      this.init
//      |        this
//      |        target
//      |      this
//      |
//      |  [this another_string] > constructor_3
//      |    seq > @
//      |      this.init
//      |        this
//      |        another_string.str
//      |      this
//      |""".stripMargin,
//    """
//      |+package stdlib.lang
//      |+alias stdlib.lang.class__Object
//      |+alias stdlib.io.class__PrintStream
//      |
//      |[] > class__System
//      |  class__Object > super
//      |  super > @
//      |
//      |  class__PrintStream.constructor > out
//      |    class__PrintStream.new
//      |
//      |  [] > new
//      |    [] > this
//      |      class__Object.new > super
//      |      super > @
//      |      "class__System" > className
//      |
//      |      [this] > init
//      |        0 > @
//      |
//      |    seq > @
//      |      this
//      |
//      |  [this] > constructor
//      |    seq > @
//      |      this.init
//      |        this
//      |      this
//      |""".stripMargin,
//    """
//      |+package stdlib.util
//      |+alias stdlib.lang.class__Object
//      |+alias stdlib.primitives.prim__int
//      |+alias stdlib.primitives.prim__float
//      |
//      |[] > class__Random
//      |  class__Object > super
//      |  super > @
//      |
//      |  [] > new
//      |    [] > this
//      |      class__Object.new > super
//      |      super > @
//      |      "class__Random" > className
//      |
//      |      [this] > init
//      |        0 > @
//      |
//      |      [this] > nextInt
//      |        random > r
//      |        seq > @
//      |          prim__int.constructor_2
//      |            prim__int.new
//      |            as-int.
//      |              sub.
//      |                mul.
//      |                  4294967295.0
//      |                  r
//      |                2147483648.0
//      |
//      |      [this] > nextFloat
//      |        random > r
//      |        seq > @
//      |          prim__float.constructor_2
//      |            prim__float.new
//      |            r
//      |
//      |    seq > @
//      |      this
//      |
//      |  [self] > constructor
//      |    seq > @
//      |      this.init
//      |        self
//      |      self
//      |""".stripMargin,
  )

}
