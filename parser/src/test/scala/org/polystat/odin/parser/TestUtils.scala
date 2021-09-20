package org.polystat.odin.parser

import com.github.tarao.nonempty.collection.NonEmpty
import pprint.PPrinter

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters._

object TestUtils {

  case class TestCase[AST](label: String, code: String, ast: Option[AST] = None)

  val astPrinter: PPrinter = pprint.copy(
    additionalHandlers = {
      case nonEmpty: NonEmpty[_, _] => pprint.treeify(nonEmpty.value)
      case s: String => pprint.Tree.Literal(s)
    }
  )

  def fileNameOf(source: String): String = {
    Paths.get(source).getFileName.toString
  }

  def readCodeFrom(fileName: String): String = {
    val code = io.Source.fromFile(fileName)
    try code.mkString finally code.close()
  }

  def getListOfFiles(dir: String): List[String] = {
    val path = getClass.getResource(dir).toURI
    Files.list(Paths.get(path)).iterator().asScala.map(_.toString).toList
  }

}
