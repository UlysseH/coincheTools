import cats.effect.{ExitCode, IO, IOApp}
import core.game.{Game, roundTree}
import core.game.cards.{Card, Deck, DistributePattern, Hand, Suit}
import core.game.cards.DistributePattern.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import cats.implicits.*
import core.game.roundTree.GameTreeNode

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      shuffledDeck = Deck.shuffled

      /*game = Game
        .init(
          ("p1", "p2", "p3", "p4"),
          Suit.None,
          shuffledDeck,
          TwoThreeThree
        )
        .copy(trumpSuit = Spades)

      optGame = game.randomOptimizeRec(1000)

      _ <- game.printInfo
      _ <- optGame.printTricks
      _ <- optGame.printPoints*/

      // _ = Game.computeGameForEachTrumpSuit()

      /*optGames = List(
        game.copy(trumpSuit = Spades).randomOptimizeRec(1000),
        game.copy(trumpSuit = Hearts).randomOptimizeRec(1000),
        game.copy(trumpSuit = Clubs).randomOptimizeRec(1000),
        game.copy(trumpSuit = Diamonds).randomOptimizeRec(1000)
      )

      // stream = fs2.Stream.emits(optGames).covary[IO]

      print = optGames.map(game =>
        for {
          _ <- game.printTricks
          _ <- game.printPoints
        } yield ()
      )

      _ <- print.sequence*/

      hands = shuffledDeck.toHandMap

      _ <- IO.println(
        hands.map(_.getNotationBaseOrder.mkString(",")).mkString("\n")
      )

      /*handMap = Map(
        "p1" -> hands(0),
        "p2" -> hands(1),
        "p3" -> hands(2),
        "p4" -> hands(3)
      )

      res = Game
        .computeGameForEachTrumpSuit(
          ("p1", "p2", "p3", "p4"),
          handMap,
          1000
        )
        .toList
        .map((a, b) => s"$a: $b")

      _ <- IO.println(res.mkString("\n"))*/

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
