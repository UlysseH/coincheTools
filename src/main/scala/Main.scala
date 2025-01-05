import cats.effect.{ExitCode, IO, IOApp}
import core.game.{Game, Players, roundTree}
import core.game.cards.{Card, Deck, DistributePattern, Hand, Suit}
import core.game.cards.DistributePattern.*
import core.game.roundTree.{GameTreeNode, RandomGameGenerator}

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      shuffledDeck = Deck.shuffled

      draw = shuffledDeck.distribute(ThreeThreeTwo)

      handMap = Map(
        "p1" -> Hand(draw.h1.cards),
        "p2" -> Hand(draw.h2.cards),
        "p3" -> Hand(draw.h3.cards),
        "p4" -> Hand(draw.h4.cards)
      )

      game = Game(("p1", "p2", "p3", "p4"), Suit.Spades, handMap)

      /*_ <- IO.println(
        Players("p1", "p2", "p3", "p4")
          .generateAllTurnsWithPoints(
            handMap("p1"),
            handMap("p2"),
            handMap("p3"),
            handMap("p4"),
            game.trumpSuit
          )
          .map(_.print)
          .mkString("\n")
      )*/

      randomGame = RandomGameGenerator.computeRec(
        game,
        "p1",
        List.empty[(Card, String)]
      )

      _ <- IO.println("\n[info] players/cards")
      _ <- IO.println(
        handMap.toList
          .map((p, h) =>
            s"$p : ${h.cards.map(_.getNotation).mkString(",")} (${h.countPoints(Suit.Spades)}pts)"
          )
          .mkString("\n")
      )

      _ <- IO.println("\n[compute] random game")
      _ <- IO.println(
        randomGame.tricks.map(_.print).mkString("\n")
      )

      _ <- IO.println(
        s"\n[total points] ${randomGame.tricks.map(_.points).sum} | [points team 1] ${randomGame.tricks
            .filter(t => (t.wonBy == "p1") || (t.wonBy == "p3"))
            .map(_.points)
            .sum} | [points team 2] ${randomGame.tricks
            .filter(t => (t.wonBy == "p2") || (t.wonBy == "p4"))
            .map(_.points)
            .sum}"
      )

      _ <- IO.never

      numGames = 1000000

      stream2 = fs2.Stream
        .emit(
          RandomGameGenerator
            .computeRec(game, game.players._1, List.empty[(Card, String)])
        )
        .repeatN(numGames)
        .covary[IO]

      stream = fs2.Stream
        .emit(RandomGameGenerator.generateRec(game, game.trumpSuit, handMap))
        .repeatN(numGames)
        .covary[IO]

      pointsL <- stream2
        .chunkN(1000)
        .parEvalMap(8)(rG =>
          IO(fs2.Stream.emits(rG.map(_.computePoints._1).toList))
        )
        .flatten
        .compile
        .toList

      _ <- IO.println(pointsL.length)
      _ <- IO.println(pointsL.sum / pointsL.length)

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
