package org.polystat.odin.parser

import pprint.PPrinter

import java.nio.file.Files
import java.nio.file.Paths
import scala.jdk.CollectionConverters._

object TestUtils {

  case class TestCase[AST](
    label: String,
    code: String,
    ast: Option[AST] = Option.empty[AST]
  )

  val astPrinter: PPrinter = pprint.copy(
    additionalHandlers = { case s: String =>
      pprint.Tree.Literal(s)
    }
  )

  def fileNameOf(source: String): String = {
    Paths.get(source).getFileName.toString
  }

  def readCodeFrom(fileName: String): String = {
    val code = io.Source.fromFile(fileName)
    try code.mkString
    finally code.close()
  }

  def getListOfFiles(dir: String): List[String] = {
    val path = getClass.getResource(dir).toURI
    Files.list(Paths.get(path)).iterator().asScala.map(_.toString).toList
  }

}
