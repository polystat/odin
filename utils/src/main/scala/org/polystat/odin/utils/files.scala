package org.polystat.odin.utils

import cats.effect.Sync
import fs2.Stream
import fs2.io.file.Files
import fs2.io.file.Path
import fs2.text

import java.io.FileNotFoundException
import java.nio.file.Paths

object files {

  def readCodeFrom[F[_]: Sync: Files](fileName: String): F[String] = {
    readCodeFrom(Path(fileName))
  }

  def readCodeStreamFrom[F[_]: Files](path: Path): Stream[F, String] = {
    Files[F]
      .readAll(path)
      .through(text.utf8.decode)
  }

  def readCodeFrom[F[_]: Sync: Files](path: Path): F[String] = {
    readCodeStreamFrom(path)
      .compile
      .string
  }

  def resourceAsPath[F[_]: Sync](
    classPathResource: String
  ): Stream[F, Path] =
    Stream.fromEither[F](
      Option(getClass.getResource(classPathResource))
        .map(p => Path.fromNioPath(Paths.get(p.toURI)))
        .toRight(
          new FileNotFoundException(
            s"No file '$classPathResource' in test resources for package ${getClass.getPackage.getName}"
          )
        )
    )

  def readEoCodeFromResources[F[_]: Sync: Files](
    dir: String
  ): F[List[(String, String)]] =
    files
      .resourceAsPath[F](dir)
      .flatMap(Files[F].walk)
      .filter(p => p.extName == ".eo")
      .flatMap(p =>
        readCodeStreamFrom(p).map(code => (p.fileName.toString, code))
      )
      .compile
      .toList

}
