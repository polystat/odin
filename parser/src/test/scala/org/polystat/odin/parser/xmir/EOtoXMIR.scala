package org.polystat.odin.parser.xmir

import cats.effect.{Resource, Sync}
import cats.implicits._
import com.jcabi.xml.XMLDocument
import org.cactoos.io.{InputOf, OutputTo}
import org.eolang.parser.{Spy, Syntax, Xsline}

import java.nio.charset.StandardCharsets
import java.nio.file.Files

private trait ParseXMIRF[F[_]] {
  def parseXMIR(code: String): F[String]
}

private object ParseXMIRF {

  implicit def live[F[_]: Sync]: ParseXMIRF[F] = new ParseXMIRF[F] {

    override def parseXMIR(code: String): F[String] = for {
      codeFile <- Sync[F].delay(
        Files.createTempFile("eo_code", ".eo")
      )
      _ <- Sync[F].delay(
        Files.write(codeFile, code.getBytes(StandardCharsets.UTF_8))
      )
      xmlFile <- Sync[F].delay(
        Files.createTempFile("eo_code", ".xml")
      )
      _ <- Sync[F].delay(
        new Syntax(
          "eo_code",
          new InputOf(codeFile),
          new OutputTo(xmlFile)
        ).parse()
      )
      _ <- Sync[F].delay(
        new Xsline(
          new XMLDocument(xmlFile),
          new OutputTo(xmlFile),
          new Spy.Verbose
        ).pass()
      )
      resource = Resource.make(
        Sync[F].delay(io.Source.fromFile(xmlFile.toAbsolutePath.toString))
      )(file => Sync[F].delay(file.close()))
      xmir <- resource.use(lines => Sync[F].pure(lines.mkString))
    } yield xmir

  }

}

object EOtoXMIR {

  def parse[F[_]: Sync](code: String): F[String] = {
    ParseXMIRF.live[F].parseXMIR(code)
  }

}
