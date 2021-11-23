package org.polystat.odin.utils

import cats.effect.Sync
import fs2.text
import fs2.Stream
import fs2.io.file.{Files, Path}

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

  def readCodeFromDir[F[_]: Sync: Files](
    dir: String
  ): F[List[(String, String)]] =
    Files[F]
      .list(Path(dir))
      .flatMap(p => readCodeStreamFrom(p).map(code => (p.toString, code)))
      .compile
      .toList

}
