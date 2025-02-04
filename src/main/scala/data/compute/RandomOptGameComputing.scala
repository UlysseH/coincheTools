package data.compute

import io.circe.generic.auto.*
import io.circe.syntax.*
import cats.implicits.*
import cats.effect.IO
import core.game.GameV2
import fs2.io.file.{Files, Path}
import fs2.text

object RandomOptGameComputing {
  def computeAndPersistN(
      cores: Int,
      n: Int,
      ts: Long,
      precision: Int,
      chunkSize: Int = 4
  ): IO[Unit] = fs2.Stream
    .range(1, n)
    .covary[IO]
    .debug()
    .chunkN(chunkSize)
    .parEvalMap(cores)(chunk => {
      IO(
        fs2.Stream
          .emits(
            chunk
              .map(_ => GameV2.randomHandMapAllSuitsToGameResultStr(precision))
              .toList
              .flatten
          )
          .covary[IO]
      )
    })
    .flatten
    .map(x => x)
    .map(gameResult => gameResult.asJson.toString)
    .debug()
    .intersperse("\n")
    .through(text.utf8.encode)
    .through(
      Files[IO]
        .writeAll(
          Path(
            s"datasets/computedOptGames/$ts.json"
          )
        )
    )
    .compile
    .drain
}
