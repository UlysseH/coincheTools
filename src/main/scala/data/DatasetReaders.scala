package data

import cats.effect.IO
import core.game.GameV2.GameResultStr
import core.game.cards.neutral.HandNeutral
import core.game.cards.{Card, Hand, Suit}
import core.game.result.ResultAllSuits
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.generic.auto.*
import fs2.data.json.*
import fs2.data.json.circe.*

import scala.concurrent.duration.*

object DatasetReaders {
  def readComputedOptGames: fs2.Stream[IO, fs2.Stream[IO, GameResultStr]] = {
    Files[IO]
      .list(Path("datasets/computedOptGames"))
      .map(path =>
        Files[IO]
          .readUtf8Lines(path)
          .map(_.stripMargin)
          .through(ast.parse)
          .map(_.as[GameResultStr])
          .collect { case Right(value) =>
            value
          }
      )
  }

  def readResultAllSuits(fileName: String): fs2.Stream[IO, ResultAllSuits] =
    Files[IO]
      .readUtf8Lines(
        Path(s"datasets/computedResultsAllSuits/$fileName.csv")
      )
      .map(ResultAllSuits.fromString)

  def readNeutralHandPoints(fileName: String) = Files[IO]
    .readUtf8Lines(
      Path(s"datasets/neutralHandPoints/$fileName.csv")
    )
    .map(_.split(":"))
    .collect {
      case t if (t.length == 2) => (t(0), t(1))
    }
    .map(t => (HandNeutral.fromString(t._1), t._2.toInt))
}
