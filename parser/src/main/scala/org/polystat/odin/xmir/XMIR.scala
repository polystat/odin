package org.polystat.odin.xmir

import cats.effect.{ExitCode, IO, IOApp, Resource, Sync}
import com.jcabi.xml.XMLDocument
import org.cactoos.io.OutputTo

import scala.xml.Node
import org.eolang.parser.{Spy, Xsline}

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.jdk.CollectionConverters._

object XMIR extends IOApp {

  val code: String =
    """+package sandbox
      |+alias stdout org.eolang.io.stdout
      |+alias sprintf org.eolang.txt.sprintf
      |
      |[] > base
      |  memory > x
      |  [self v] > f
      |    x.write > @
      |      v
      |  [self v] > g
      |    self.f > @
      |      self
      |      v
      |[] > derived
      |  base > @
      |  [self v] > f
      |    self.g > @
      |      self
      |      v
      |[a]
      |  [] > name
      |[]
      |  [] > a
      |  [] > name
      |[a name]
      |add. > zyx
      |  1
      |  2
      |if.
      |  less.
      |    1
      |    2
      |  true
      |  false
      |""".stripMargin

  val divByZero: String =
    """[] > base
      |  2 > a
      |  [self x...] > f
      |    div. > @
      |      x.get 0
      |      self.a
      |[] > derived
      |  base > @
      |  0 > a
      |  base.^.f > stuff
      |""".stripMargin

  def parseXMIR(xmir: Node) = {
    val objs = xmir \\ "objects"
    objs.map(parseObject)
  }

  def parseObject(obj: Node) = {
    obj
    // abstract object - opt[parent]
    //                   opt[name],
    //                   parent exists <=> parent@line != self@line
    // free attribute - parent, parent@line == self@line, name

  }

  override def run(args: List[String]): IO[ExitCode] = {
    for {
      xmir <- EOtoXMIR.parse[IO](divByZero)
      _ <- IO.println(scala.xml.XML.loadString(xmir) \\ "objects")
      jcabiXml <- IO.pure(new XMLDocument(xmir))
      out <- Sync[IO].delay(Files.createTempFile("xmir", ".xml"))
      _ <-
        Sync[IO].delay(Files.write(out, xmir.getBytes(StandardCharsets.UTF_8)))
      _ <- Sync[IO].delay(
        new Xsline(
          jcabiXml,
          new OutputTo(out),
          new Spy.Verbose,
          List("simplify-xmir.xsl").asJava
        ).pass()
      )
      file = Resource.make(Sync[IO].delay(io.Source.fromFile(out.toUri)))(
        file => Sync[IO].delay(file.close())
      )
      betterXmir <- file.use(xml => IO.pure(xml.mkString))
      scalaXml <- IO.pure(scala.xml.XML.loadString(betterXmir))
      parsed <- IO.pure(parseXMIR(scalaXml))
      _ <- IO.println(parsed)
    } yield ExitCode.Success
  }

}
