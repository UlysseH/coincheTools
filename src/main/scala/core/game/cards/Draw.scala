package core.game.cards

import core.game.roundTree.Tricks

case class Draw(h1: Hand, h2: Hand, h3: Hand, h4: Hand) {
  def toStringList: (String, String, String, String) =
    (h1.toString, h2.toString, h3.toString, h4.toString)
  def toStringTrumpOrder
      : (List[String], List[String], List[String], List[String]) = (
    h1.toStringTrumpOrder,
    h2.toStringTrumpOrder,
    h3.toStringTrumpOrder,
    h4.toStringTrumpOrder
  )
  def toStringBaseOrder
      : (List[String], List[String], List[String], List[String]) =
    (
      h1.toStringBaseOrder,
      h2.toStringBaseOrder,
      h3.toStringBaseOrder,
      h4.toStringBaseOrder
    )

  def generateAllTurnsWithPoints(
      trumpSuit: Suit
  ) = {
    h1.cards.flatMap(c1 =>
      h2.cards.flatMap(c2 =>
        h3.cards.flatMap(c3 =>
          h4.cards.flatMap(c4 =>
            // Tricks.fromCards(List((c1, )))
            List(
              (c1, c2, c3, c4),
              (c2, c3, c4, c1),
              (c3, c4, c1, c2),
              (c4, c1, c2, c3)
            )
          )
        )
      )
    )
  }

}
