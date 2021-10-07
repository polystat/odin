package org.polystat.odin.xmir

import cats.effect.Sync
import cats.implicits._
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import javax.xml.transform.TransformerFactory
import javax.xml.transform.stream.{StreamResult, StreamSource}
import scala.xml.Elem

object XMIR {

  def applyXSLT[F[_]: Sync](xml: Path): F[Elem] =
    for {
      in <- Sync[F].delay(
        new StreamSource(
          new ByteArrayInputStream(Files.readAllBytes(xml))
        )
      )
      xsl <- Sync[F].delay(
        new StreamSource(
          getClass.getClassLoader.getResourceAsStream("simplify-xmir.xsl")
        )
      )
      out <- Sync[F].delay(
        new StreamResult(
          Files.newOutputStream(xml)
        )
      )

      transformer <- Sync[F].delay(
        TransformerFactory.newInstance().newTransformer(xsl)
      )
      _ <- Sync[F].delay(
        transformer.transform(in, out)
      )
      scalaXML <- Sync[F].delay(
        scala.xml.XML.loadFile(xml.toAbsolutePath.toString)
      )
    } yield scalaXML

  def parse[F[_]: Sync](xmir: String): F[Vector[EOBnd[EOExprOnly]]] = {
    for {
      out <- Sync[F].delay(Files.createTempFile("xmir", ".xml"))
      _ <- Sync[F].delay(
        Files.write(out, xmir.getBytes(StandardCharsets.UTF_8))
      )
      scalaXML <- applyXSLT(out)
      objs <- Sync[F].delay(
        (scalaXML \\ "objects" \ "_")
          .collect { case elem: Elem => elem }
      )
      parsed <- Sync[F].delay(
        objs.map(node => bndFromTuple(parseObject(node))).toVector
      )
    } yield parsed
  }

  def cleanName(name: String): String = {
    val parts = name.split("\\.")
    parts(parts.length - 1)
  }

  def extractName(attrMap: Map[String, String]): Option[EONamedBnd] = {
    (attrMap.get("bound-to"), attrMap.get("const")) match {
      case (Some("@"), _) => Some(EODecoration)
      case (Some(name), Some(_)) =>
        Some(EOAnyNameBnd(ConstName(name)))
      case (Some(name), None) =>
        Some(EOAnyNameBnd(LazyName(name)))
      case (None, _) => None
    }
  }

  def bndFromTuple: ((Option[EONamedBnd], EOExprOnly)) => EOBnd[EOExprOnly] = {
    case (Some(name), value) => EOBndExpr(name, value)
    case (None, value) => EOAnonExpr(value)
  }

  def namedBndFromTuple: ((Option[EONamedBnd], EOExprOnly)) => EOBndExpr[EOExprOnly] = {
    case (Some(name), expr) => EOBndExpr(name, expr)
    case (None, _) => throw new Exception("Named binding expected!")
  }

  def parseObject(obj: Elem): (Option[EONamedBnd], EOExprOnly) = {
    val filtered = obj
    filtered match {
      case Elem(_, "data", attrs, _, _ @_*) => {
        val attrMap = attrs.asAttrMap
        val name: Option[EONamedBnd] = extractName(attrMap)
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
            case (Some("float"), Some(value)) => EOFloatData(value.toFloat)
            case (dataType, value) => throw new Exception(
                s"data has unknown signature: $dataType $value"
              )
          }
        )
        (name, value)
      }
      case Elem(_, "copy", attrs, _, children @ _*) => {
        val name = extractName(attrs.asAttrMap)
        val of = children
          .find(node => node.label == "of")
          .get
          .child
          .collect { case elem: Elem => elem }
          .head
        val `with` =
          children
            .find(node => node.label == "with")
            .get
            .child
            .collect { case elem: Elem => elem }

        // TODO: Where does trgName go???
        val (_, trg) = parseObject(of)

        NonEmpty.from(`with`.map(parseObject).toVector) match {
          case Some(value) =>
            (name, Fix[EOExpr](EOCopy(trg, value.map(bndFromTuple))))
          case None => (name, trg)
        }
      }
      case Elem(_, "simple-app", attrs, _, _ @_*) => {
        val name = cleanName(attrs.asAttrMap("name"))
        (None, Fix[EOExpr](EOSimpleApp(name)))
      }
      case Elem(_, "attribute", attrs, _, children @ _*) => {
        val name = attrs.asAttrMap("name")
        val (_, expr) =
          parseObject(
            children
              .find(_.label == "of")
              .get
              .child
              .collect { case elem: Elem => elem }
              .head
          )
        (None, Fix[EOExpr](EODot(expr, name)))
      }
      case Elem(_, "abstraction", attrs, _, children @ _*) => {
        val name = extractName(attrs.asAttrMap)
        val (varargSeq, freeSeq) = children
          .collect { case elem: Elem => elem }
          .filter(_.label == "free")
          .partition(node => node.attributes.get("vararg").nonEmpty)
        val vararg = varargSeq
          .map(node => LazyName(node \@ "name"))
          .headOption
        val free = freeSeq
          .map(node => LazyName(node \@ "name"))
          .toVector
        val boundAttrs = children
          .collect { case elem: Elem => elem }
          .filter(_.label != "free")
          .map(node => namedBndFromTuple(parseObject(node)))
          .toVector
        (name, Fix[EOExpr](EOObj(free, vararg, boundAttrs)))
      }
      case elem: Elem => {
        throw new Exception(elem.toString)
      }
    }
  }

}
