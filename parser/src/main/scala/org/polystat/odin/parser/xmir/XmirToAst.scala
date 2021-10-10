package org.polystat.odin.parser.xmir

import cats.effect.Sync
import cats.implicits._
import com.github.tarao.nonempty.collection.NonEmpty
import higherkindness.droste.data.Fix
import org.polystat.odin.core.ast._
import org.polystat.odin.core.ast.astparams.EOExprOnly
import org.xml.sax.InputSource

import java.io.{ByteArrayInputStream, StringReader, StringWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.{StreamResult, StreamSource}
import scala.xml.Elem
import scala.xml.XML

trait XmirToAst[F[_], T] {
  /**
    * Accepts a Seq of XML elements encoded as UTF-8 strings and parses them
    * into a Vector of EOBnd from org.polystat.odin.core.ast
    *
    * @param objs
    *   a sequence of XML XMIR objects encoded as UTF-8 strings
    * @return
    *   Either an error message, or a Vector of EOBnds
    */
  def parseXMIR(objs: T): F[Either[String, Vector[EOBnd[EOExprOnly]]]]
}

object XmirToAst {

  def apply[F[_], T](implicit impl: XmirToAst[F, T]): XmirToAst[F, T] = impl

  implicit def string[F[_]: Sync, T](implicit
    seqString: XmirToAst[F, Seq[String]]
  ): XmirToAst[F, String] =
    new XmirToAst[F, String] {

      override def parseXMIR(
        objs: String
      ): F[Either[String, Vector[EOBnd[EOExprOnly]]]] = {
        for {
          scalaXML <- Sync[F].delay(
            (XML.loadString(objs) \\ "objects" \ "o")
              .collect { case elem: Elem => elem.toString }
          )
          parsed <- seqString.parseXMIR(scalaXML)
        } yield parsed
      }

    }

  implicit def seqString[F[_]: Sync]: XmirToAst[F, Seq[String]] =
    new XmirToAst[F, Seq[String]] {

      override def parseXMIR(
        xmir: Seq[String]
      ): F[Either[String, Vector[EOBnd[EOExprOnly]]]] = {
        for {
          out <- Sync[F].delay(Files.createTempFile("xmir", ".xml"))
          _ <- Sync[F].delay {
            Files.write(out, buildXML(xmir).getBytes(StandardCharsets.UTF_8))
          }
          _ <- applyXSLT(out)
          scalaXML <- Sync[F].delay(
            (XML.loadFile(out.toAbsolutePath.toString) \\ "objects" \ "_")
              .collect { case elem: Elem => elem }
          )
          parsed = combineErrors(
            scalaXML.map(obj => parseObject(obj).map(bndFromTuple))
          )
        } yield parsed.map(_.toVector)
      }

      /**
        * adapted from
        * https://stackoverflow.com/questions/2567416/xml-document-to-string
        *
        * @param elems
        *   A sequence of XML elements as UTF-8 strings
        * @return
        *   a single XML document as UTF-8 string of format
        *   <objects>{elems}</objects>
        */
      private[this] def buildXML(elems: Seq[String]): String = {
        val doc = DocumentBuilderFactory
          .newInstance
          .newDocumentBuilder
          .newDocument
        val root = doc.createElement("objects")
        elems
          .map(elem => {
            DocumentBuilderFactory
              .newInstance
              .newDocumentBuilder
              .parse(new InputSource(new StringReader(elem)))
              .getFirstChild
          })
          .foreach(node => root.appendChild(doc.adoptNode(node)))
        doc.appendChild(root)
        val sw: StringWriter = new StringWriter
        val transformer = TransformerFactory.newInstance.newTransformer
        transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "no")
        transformer.setOutputProperty(OutputKeys.METHOD, "xml")
        transformer.setOutputProperty(OutputKeys.INDENT, "yes")
        transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
        transformer.transform(new DOMSource(doc), new StreamResult(sw))
        sw.toString
      }

      /**
        * adapted from
        *
        * https://stackoverflow.com/questions/5977846/how-to-apply-xsl-to-xml-in-java
        *
        * @param xml
        *   Path to an XML document which is to be transformed
        * @return
        *   Transform the document with `simple-xmir.xsl` and write it back to
        *   `xml`
        */
      private[this] def applyXSLT(xml: Path): F[Unit] =
        for {
          in <- Sync[F].delay(
            new StreamSource(
              new ByteArrayInputStream(Files.readAllBytes(xml))
            )
          )
          xsl <- Sync[F].delay(
            new StreamSource(
              getClass
                .getClassLoader
                .getResourceAsStream("simplify-xmir.xsl")
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
        } yield ()

      private[this] def cleanName(name: String): Option[String] = {
        val parts = name.split("\\.")
        parts.lastOption
      }

      private[this] def extractName(
        attrMap: Map[String, String]
      ): Option[EONamedBnd] = {
        (attrMap.get("bound-to"), attrMap.get("const")) match {
          case (Some("@"), _) => Some(EODecoration)
          case (Some(name), Some(_)) =>
            Some(EOAnyNameBnd(ConstName(name)))
          case (Some(name), None) =>
            Some(EOAnyNameBnd(LazyName(name)))
          case (None, _) => None
        }
      }

      private[this] def combineErrors[A](
        eithers: collection.Seq[Either[String, A]]
      ): Either[String, collection.Seq[A]] = {
        (eithers.partitionMap(identity) match {
          case (Nil, rights) => Right(rights)
          case (lefts, _) => Left(lefts)
        }).leftMap(_.mkString("\n"))
      }

      private[this] val bndFromTuple: ((Option[EONamedBnd], EOExprOnly)) => EOBnd[EOExprOnly] = {
        case (Some(name), value) => EOBndExpr(name, value)
        case (None, value) => EOAnonExpr(value)
      }

      private[this] val namedBndFromTuple: Either[String, (Option[EONamedBnd], EOExprOnly)] => Either[String, EOBndExpr[EOExprOnly]] = {
        case Right((Some(name), expr)) => Right(EOBndExpr(name, expr))
        case Right((None, _)) => Left("Expected a named binding!")
        case Left(msg) => Left(msg)
      }

      private[this] def parseObject(
        obj: Elem
      ): Either[String, (Option[EONamedBnd], EOExprOnly)] = {
        obj match {
          case Elem(_, "data", attrs, _, _*) =>
            val attrMap = attrs.asAttrMap
            val name: Option[EONamedBnd] = extractName(attrMap)
            val value: Either[String, EOExprOnly] =
              (attrMap.get("type"), attrMap.get("value")) match {
                case (Some("int"), Some(value)) =>
                  Right(Fix[EOExpr](EOIntData(value.toInt)))
                case (Some("bool"), Some(value)) => {
                  val bool: Either[String, Boolean] = value match {
                    case "true" => Right(true)
                    case "false" => Right(false)
                    case other => Left(
                        s"Boolean has a value which is not \"true\" or \"false\": $other"
                      )
                  }
                  bool.map(value => Fix[EOExpr](EOBoolData(value)))
                }
                case (Some("string"), Some(value)) =>
                  Right(Fix[EOExpr](EOStrData(value)))
                case (Some("char"), Some(value)) =>
                  Right(Fix[EOExpr](EOCharData(value.charAt(0))))
                case (Some("float"), Some(value)) =>
                  Right(Fix[EOExpr](EOFloatData(value.toFloat)))
                case (dataType, value) => Left(
                    s"Unknown data encountered: $value of type $dataType"
                  )
              }
            value.map(expr => (name, expr))
          case Elem(_, "copy", attrs, _, children @ _*) =>
            val name = extractName(attrs.asAttrMap)
            val of = Either
              .fromOption(
                children
                  .find(node => node.label == "of"),
                "\"copy\" element doesn't contain \"of\" element!"
              )
              .map(of => of.child.collect { case elem: Elem => elem }.head)
            val `with` = Either
              .fromOption(
                children
                  .find(node => node.label == "with"),
                "\"copy\" element doesn't contain \"with\" element!"
              )
              .map(of => of.child.collect { case elem: Elem => elem })
            val parsedOf = of.map(of => parseObject(of))
            val parsedWith = `with`
              .map(_.map(parseObject))

            for {
              eitherOf <- parsedOf
              (_, trg) <- eitherOf
              eitherWith <- parsedWith
              combineWith = combineErrors(eitherWith)
              wth <- combineWith
            } yield NonEmpty.from(wth.toVector) match {
              case Some(value) =>
                (name, Fix[EOExpr](EOCopy(trg, value.map(bndFromTuple))))
              case None => (name, trg)
            }
          case Elem(_, "simple-app", attrs, _, _*) =>
            val name: Either[String, String] = Either.fromOption(
              attrs.asAttrMap.get("name").flatMap(cleanName),
              "\"simple-app\" element has no attribute \"name\"!"
            )
            for {
              name <- name
            } yield (None, Fix[EOExpr](EOSimpleApp(name)))
          case Elem(_, "attribute", attrs, _, children @ _*) =>
            val name: Either[String, String] = Either.fromOption(
              attrs.asAttrMap.get("name"),
              "\"attribute\" element doesn't have a \"name\" attribute!"
            )
            val of = Either.fromOption(
              children.find(_.label == "of"),
              "\"attribute\" element has no child element \"of\""
            )
            val parsedOf = {
              of.map(of =>
                parseObject(
                  of
                    .child
                    .collect { case elem: Elem => elem }
                    .head
                )
              )
            }
            for {
              name <- name
              of <- parsedOf
              (_, expr) <- of
            } yield (None, Fix[EOExpr](EODot(expr, name)))
          case Elem(_, "abstraction", attrs, _, children @ _*) =>
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
            val boundAttrs = combineErrors(
              children
                .collect { case elem: Elem => elem }
                .filter(_.label != "free")
                .map(node => namedBndFromTuple(parseObject(node)))
            ).map(_.toVector)
            for {
              bndAttrs <- boundAttrs
            } yield (name, Fix[EOExpr](EOObj(free, vararg, bndAttrs)))
          case elem: Elem => Left(s"An unknown element encountered: $elem")
        }
      }

    }

  def parseXMIR[F[_], T](
    objs: T
  )(implicit
    impl: XmirToAst[F, T]
  ): F[Either[String, Vector[EOBnd[EOExprOnly]]]] =
    XmirToAst[F, T].parseXMIR(objs)

}
