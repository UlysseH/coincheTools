package core.game.roundTree

import core.game.Player
import core.game.cards.{Card, Suit}

case class Tricks(
    wonBy: Player,
    points: Int,
    cards: List[Card],
    firstPlayer: Player
) {
  def print =
    s"[wonBy] $wonBy | [points] $points | ${cards.map(_.getNotation).mkString(",")} | [firstPlayer] $firstPlayer"

}

object Tricks {
  def fromCards(
      playedCards: List[(Card, Player)],
      trumpSuit: Suit,
      isLast: Boolean = false
  ): Tricks = {
    val wonBy =
      if (playedCards.exists(_._1.suit.==(trumpSuit)))
        playedCards
          .filter(_._1.suit.==(trumpSuit))
          .maxBy(_._1.height.getTrumpRank)
          ._2
      else
        playedCards
          .filter(_._1.suit.==(playedCards.head._1.suit))
          .maxBy(_._1.height.getBaseRank)
          ._2

    val points = playedCards
      .map((card, _) =>
        if (card.suit == trumpSuit) card.height.getTrumpPoints
        else card.height.getBasePoints
      )
      .sum

    Tricks(
      wonBy,
      if (isLast) points + 10 else points,
      playedCards.map(_._1),
      playedCards.head._2
    )
  }
}
