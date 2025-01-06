import cats.effect.{ExitCode, IO, IOApp}
import core.game.{Game, roundTree}
import core.game.cards.{Card, Deck, DistributePattern, Hand, Suit}
import core.game.cards.DistributePattern.*
import core.game.roundTree.GameTreeNode

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

      game = Game.init(
        ("p1", "p2", "p3", "p4"),
        Suit.Spades,
        shuffledDeck,
        TwoThreeThree
      )

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

      randomGame = game.generateRandomGameFromHere

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
        game.tricksList.map(_.print).mkString("\n")
      )

      _ <- IO.println(
        s"\n[total points] ${randomGame.tricksList.map(_.points).sum} | [points team 1] ${randomGame.tricksList
            .filter(t => (t.wonBy == "p1") || (t.wonBy == "p3"))
            .map(_.points)
            .sum} | [points team 2] ${randomGame.tricksList
            .filter(t => (t.wonBy == "p2") || (t.wonBy == "p4"))
            .map(_.points)
            .sum}"
      )

      _ <- IO.never
      /*
      numGames = 1000000

      stream2 = fs2.Stream
        .emit(
          game.generateRandomGameFromHere
        )
        .repeatN(numGames)
        .covary[IO]

      stream = fs2.Stream
        .emit(game.generateRandomGameFromHere)
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
      _ <- IO.println(pointsL.sum / pointsL.length)*/

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
