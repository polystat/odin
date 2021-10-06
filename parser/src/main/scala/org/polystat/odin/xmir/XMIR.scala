package org.polystat.odin.xmir

import cats.effect._
import com.jcabi.xml.XMLDocument
import higherkindness.droste.data.Fix
import org.cactoos.io.OutputTo
import org.eolang.parser.{Spy, Xsline}
import org.polystat.odin.core.ast._

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import scala.jdk.CollectionConverters._
import scala.xml.{Elem, Node}

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
      |  2.5 > a
      |  [self x...] > f
      |    div. > @
      |      x.get 0
      |      self.a
      |[] > derived
      |  base > @
      |  0 > a
      |  base.^.f > stuff
      |true > aTrue
      |false > aFalse
      |true
      |false
      |"str" > str
      |'c' > char
      |123
      |123 > one-two-three
      |a > anA
      |a
      |""".stripMargin

  def parseXMIR(xmir: Node) = {
    val objs = xmir \\ "objects" \ "_"
    println(objs.length)
    objs.map(parseObject)
  }

  def parseObject(obj: Node) = {
    obj match {
      case Elem(null, "data", attrs, _, _ @_*) => {
        val attrMap = attrs.asAttrMap
        val name: Option[EONamedBnd] =
          (attrMap.get("bound-to"), attrMap.get("const")) match {
            case (Some("@"), _) => Some(EODecoration)
            case (Some(name), Some(_)) =>
              Some(EOAnyNameBnd(ConstName(name)))
            case (Some(name), None) =>
              Some(EOAnyNameBnd(LazyName(name)))
            case (None, _) => None
          }
        val value = Fix[EOExpr](
          (attrMap.get("type"), attrMap.get("value")) match {
            case (Some("int"), Some(value)) => EOIntData(value.toInt)
            case (Some("bool"), Some(value)) => EOBoolData(value match {
                case "true" => true
                case "false" => false
                case other =>
                  throw new Exception(s"illegal boolean value: $other")
              })
            case (Some("string"), Some(value)) => EOStrData(value)
            case (Some("char"), Some(value)) => EOCharData(value.charAt(0))
            case (dataType, value) => throw new Exception(
                s"data has unknown signature: $dataType $value"
              )
          }
        )
        name match {
          case Some(name) => EOBndExpr(name, value)
          case None => EOAnonExpr(value)
        }
      }
      case _ => obj
    }
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
      _ <- IO.println(betterXmir)
      scalaXml <- IO.pure(scala.xml.XML.loadString(betterXmir))
      parsed <- IO.pure(parseXMIR(scalaXml))
      _ <- IO.println(parsed)
    } yield ExitCode.Success
  }

}
