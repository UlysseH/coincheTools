import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto.*
import io.circe.syntax.*
import core.game.{Game, GameV2, Player, roundTree}
import core.game.cards.{Card, Deck, DistributePattern, Hand, HandMap, Suit}
import core.game.cards.DistributePattern.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import cats.implicits.*
import core.game.Player.*
import core.game.Team.teamA
import core.game.bidding.Contract
import core.game.roundTree.{GameTreeNode, Tricks}
import fs2.io.file.{Files, Path}
import fs2.text

import scala.math.BigDecimal.RoundingMode

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      // games = GameV2.randomHandMapAllSuitsToGameResultStr(1000)

      computeStream = fs2.Stream
        .range(1, 20000)
        .covary[IO]
        .debug()
        .chunkN(4)
        .parEvalMap(10)(chunk => {
          IO(
            fs2.Stream
              .emits(
                chunk
                  .map(_ => GameV2.randomHandMapAllSuitsToGameResultStr(1000))
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
                s"datasets/computedOptGames/$start.json"
              )
            )
        )

      _ <- computeStream.compile.drain

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
