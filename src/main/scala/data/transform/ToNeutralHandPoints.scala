package data.transform

import cats.effect.IO
import core.game.cards.neutral
import core.game.cards.neutral.HandNeutral
import data.DatasetReaders
import fs2.io.file.{Files, Path}
import fs2.text

object ToNeutralHandPoints {
  def fromResultsAllSuits(fileName: String): fs2.Stream[IO, (HandNeutral, Int)] = DatasetReaders
    .readResultAllSuits(fileName)
    .flatMap(res =>
      fs2.Stream
        .emits[IO, (HandNeutral, Int)](
          res.pointsMap.toList
            .map((suit, pts) => (HandNeutral.fromHand(res.hand, suit), pts))
        )
    )

  def writeToFile(
      stream: fs2.Stream[IO, (HandNeutral, Int)],
      fileName: String
  ): IO[Unit] = stream
    .map((hand, pts) => s"${hand.toString}:$pts")
    .intersperse("\n")
    .through(text.utf8.encode)
    .through(
      Files[IO].writeAll(Path(s"datasets/neutralHandPoints/$fileName.csv"))
    )
    .compile
    .drain
}
