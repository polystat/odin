package org.polystat.odin.parser

import com.github.tarao.nonempty.collection.NonEmpty
import pprint.PPrinter

object TestUtils {

  case class TestCase[AST](
    label: String,
    code: String,
    ast: Option[AST] = Option.empty[AST]
  )

  val astPrinter: PPrinter = pprint.copy(
    additionalHandlers = {
      case nonEmpty: NonEmpty[_, _] => pprint
          .treeify(
            x = nonEmpty.value,
            escapeUnicode = false,
            showFieldNames = true
          )
      case s: String => pprint.Tree.Literal(s)
    }
  )

}
