package data

import cats.effect.IO
import core.game.GameV2.GameResultStr
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.generic.auto.*
import io.circe.fs2.*
import scala.concurrent.duration._

object DatasetReaders {
  def readComputedOptGames = {
    val jsonStream = Files[IO]
      .list(Path("datasets/computedOptGames"))
      .flatMap(path => Files[IO].readUtf8Lines(path))
      .metered(1000.millis)
    // .through(stringArrayParser)
    val resStream = jsonStream
      .debug()
    // .through(decoder[IO, GameResultStr])
    resStream
  }
}
