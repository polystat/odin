package org.polystat.odin.utils

import cats.effect.Sync
import fs2.io.file.Path
import fs2.io.readClassResource
import fs2.text

object files {

  def readEoCodeFromDirectory[F[_]: Sync](
    dir: String
  ): F[List[(String, String)]] = {
    readClassResource(dir)
      .through(text.utf8.decode)
      .through(text.lines)
      .filter(_.nonEmpty)
      .flatMap(path =>
        readClassResource(
          (Path(dir) / Path(path))
            .toString
            .replace("\\", "/")
        )
          .through(text.utf8.decode)
          .map(code => (path, code))
      )
      .compile
      .toList
  }

}
