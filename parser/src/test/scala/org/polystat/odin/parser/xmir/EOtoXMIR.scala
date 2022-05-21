package org.polystat.odin.parser.xmir

import cats.effect.Resource
import cats.effect.Sync
import cats.implicits._
import com.jcabi.xml.XMLDocument
import org.cactoos.io.InputOf
import org.cactoos.io.OutputTo
import org.eolang.parser.Spy
import org.eolang.parser.Syntax
import org.eolang.parser.Xsline

import java.nio.charset.StandardCharsets
import java.nio.file.Files

private trait EOtoXMIRF[F[_]] {
  def parseEO(code: String): F[String]
}

private object EOtoXMIRF {

  implicit def live[F[_]: Sync]: EOtoXMIRF[F] = new EOtoXMIRF[F] {

    override def parseEO(code: String): F[String] = for {
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
    EOtoXMIRF.live[F].parseEO(code)
  }

}
