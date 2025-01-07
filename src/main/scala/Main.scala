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

      game = Game
        .init(
          ("p1", "p2", "p3", "p4"),
          Suit.None,
          shuffledDeck,
          TwoThreeThree
        )
        .copy(trumpSuit = Spades)
      /*.copy(
          handMap = Map(
            "p1" -> Hand(
              List("7s", "8d", "As", "Ad", "Qh", "Td", "Js", "Ts")
                .map(Card.fromLitteral)
            ),
            "p2" -> Hand(
              List("8h", "Qd", "Jh", "Qc", "9s", "9c", "Tc", "9h")
                .map(Card.fromLitteral)
            ),
            "p3" -> Hand(
              List("7d", "Kd", "Kh", "8c", "Jc", "Ks", "8s", "Qs")
                .map(Card.fromLitteral)
            ),
            "p4" -> Hand(
              List("Jd", "9d", "7h", "Kc", "Th", "Ac", "7c", "Ah")
                .map(Card.fromLitteral)
            )
          )
        )*/

      optGame = game.randomOptimizeRec(1000)

      _ <- game.printInfo
      _ <- optGame.printTricks
      _ <- optGame.printPoints

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

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
