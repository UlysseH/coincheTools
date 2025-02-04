/*
import cats.effect.{ExitCode, IO, IOApp}
import core.game.{Game, roundTree}
import core.game.cards.{Card, Deck, DistributePattern, Hand, Suit}
import core.game.cards.DistributePattern.*
import core.game.roundTree.GameTreeNode

object ComputeCompleteTree extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      shuffledDeck = Deck.shuffled

      draw = shuffledDeck.distribute(ThreeThreeTwo)

      cardsTrunc = 4
      handMap = Map(
        "p1" -> Hand(draw.h1.cards.take(cardsTrunc)),
        "p2" -> Hand(draw.h2.cards.take(cardsTrunc)),
        "p3" -> Hand(draw.h3.cards.take(cardsTrunc)),
        "p4" -> Hand(draw.h4.cards.take(cardsTrunc))
      )

      game = Game.init(
        ("p1", "p2", "p3", "p4"),
        Suit.Spades,
        shuffledDeck,
        ThreeThreeTwo
      )

      tree = roundTree
        .GameTreeNode(
          game,
          List.empty[GameTreeNode]
        )
        .compute

      _ <- IO.println("[compute] number of leaves")
      _ <- IO.println(tree.countLeaf)

      _ <- IO.println("\n[info] players/cards")
      _ <- IO.println(
        handMap.toList
          .map((p, h) => s"$p : ${h.cards.map(_.toString).mkString(",")}")
          .mkString("\n")
      )

      _ <- IO.println("\n[compute] leaves")
      _ <- IO.println(
        tree.getLeaves.map(_.map(_.toString).mkString(",")).mkString("\n")
      )

      _ <- IO.println("\n[compute] number of leaves")
      _ <- IO.println(tree.countLeaf)

      // _ <- IO.println(draw.toStringTrumpOrder)

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
 */
