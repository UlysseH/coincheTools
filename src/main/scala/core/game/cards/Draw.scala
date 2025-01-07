package core.game.cards

import core.game.roundTree.Tricks

case class Draw(h1: Hand, h2: Hand, h3: Hand, h4: Hand) {
  def getNotation: (List[String], List[String], List[String], List[String]) =
    (h1.getNotation, h2.getNotation, h3.getNotation, h4.getNotation)
  def getNotationTrumpOrder
      : (List[String], List[String], List[String], List[String]) = (
    h1.getNotationTrumpOrder,
    h2.getNotationTrumpOrder,
    h3.getNotationTrumpOrder,
    h4.getNotationTrumpOrder
  )
  def getNotationBaseOrder
      : (List[String], List[String], List[String], List[String]) =
    (
      h1.getNotationBaseOrder,
      h2.getNotationBaseOrder,
      h3.getNotationBaseOrder,
      h4.getNotationBaseOrder
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
