package data.transform

import cats.effect.IO
import core.game.cards.{Hand, Suit}
import core.game.result.ResultAllSuits
import data.DatasetReaders
import fs2.io.file.{Files, Path}
import fs2.text

object ToResultAllSuits {
  def fromFilesToFile(fileName: String) = for {
    _ <- IO.println("[starting]")
    data <- DatasetReaders.readComputedOptGames
      .evalMap(
        _.compile.toList
          .map(
            _.groupBy(_.id.split("-").reverse.tail.reverse.mkString("-")).toList
              .collect((_, resL) =>
                resL.headOption match
                  case Some(res) =>
                    ResultAllSuits(
                      Hand.fromString(res.cards1),
                      resL
                        .map(elt =>
                          Suit.fromString(elt.trumpSuit) -> elt.pointsA
                        )
                        .toMap
                    )
              )
          )
      )
      .compile
      .toList
      .map(_.flatten)

    outputStream <- fs2.Stream
      .emits[IO, ResultAllSuits](data)
      .map(_.toString)
      .intersperse("\n")
      .through(text.utf8.encode)
      .through(
        Files[IO].writeAll(Path(s"datasets/computedResultsAllSuits/$fileName.csv"))
      )
      .compile
      .drain
  } yield ()
}
