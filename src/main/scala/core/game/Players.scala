package core.game

import core.game.cards.{Card, Hand, Suit}
import core.game.roundTree.Tricks

case class Players(one: String, two: String, three: String, four: String) {
  def generateAllTurnsWithPoints(
      h1: Hand,
      h2: Hand,
      h3: Hand,
      h4: Hand,
      trumpSuit: Suit
  ): List[Tricks] = {
    h1.cards.flatMap(c1 =>
      h2.cards.flatMap(c2 =>
        h3.cards.flatMap(c3 =>
          h4.cards.flatMap(c4 =>
            val t1 = Tricks.fromCards(
              List((c1, one), (c2, two), (c3, three), (c4, four)),
              trumpSuit
            )
            val t2 = Tricks.fromCards(
              List((c2, two), (c3, three), (c4, four), (c1, one)),
              trumpSuit
            )
            val t3 = Tricks.fromCards(
              List((c3, three), (c4, four), (c1, one), (c2, two)),
              trumpSuit
            )
            val t4 = Tricks.fromCards(
              List((c4, four), (c1, one), (c2, two), (c3, three)),
              trumpSuit
            )
            /*List(
              (c1, c2, c3, c4, t1.wonBy, t1.points, one),
              (c2, c3, c4, c1, t2.wonBy, t2.points, two),
              (c3, c4, c1, c2, t3.wonBy, t3.points, three),
              (c4, c1, c2, c3, t4.wonBy, t4.points, four)
            )*/
            List(
              Tricks(t1.wonBy, t1.points, List(c1, c2, c3, c4), one),
              Tricks(t2.wonBy, t2.points, List(c2, c3, c4, c1), two),
              Tricks(t3.wonBy, t3.points, List(c3, c4, c1, c2), three),
              Tricks(t4.wonBy, t4.points, List(c4, c1, c2, c3), four)
            )
          )
        )
      )
    )
  }
}
