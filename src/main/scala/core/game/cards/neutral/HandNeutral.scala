package core.game.cards.neutral

import core.game.cards.neutral.SuitNeutral.{First, Second, Third, Trump}
import core.game.cards.{Card, Hand, Suit}

import scala.annotation.tailrec

// todo: add the neutral notation mapping (to compare with other players hand
case class HandNeutral(cards: List[CardNeutral]) {
  override def toString: String = cards.map(_.getNotation).mkString(",")

  def pointsInHand: Int = cards
    .map(card =>
      if (card.suitNeutral.==(Trump)) card.height.getTrumpPoints
      else card.height.getBasePoints
    )
    .sum

  /**
   *  [(J, 9, A, X), (A, T, X), (X)]
   *  [(J, X, X), (A, X), (A), (X, X)]
   *  ...
   */

}

object HandNeutral {

  /** This internal function compare two sets of cards of non-suited colors with
    * the logic (Ax) > (Ty, Ky, 7y) > (Tz, 8z) true if first is superior, false
    * if interior
    */
  // ou alors on se fait pas trop chier
  def sortColors(cardsL: List[List[Card]]): List[List[Card]] = cardsL
    .sortBy(cards =>
      cards
        .map(_.height.getBasePoints)
        .map(i => i * i * i * i)
        .sum + cards.length
    )
    .reverse

  def fromHand(hand: Hand, trumpSuit: Suit): HandNeutral = {
    val cards = hand.cards

    val trumpCardsNeutral = cards
      .filter(_.suit.==(trumpSuit))
      .map(card => CardNeutral(Trump, card.height))
      .sortBy(_.height.getTrumpRank)
      .reverse
    val restGroupedBySuit = cards
      .filterNot(_.suit.==(trumpSuit))
      .groupBy(_.suit)
      .toList
      .map((_, cards) => cards.sortBy(_.height.getBaseRank).reverse)
    val restSorted = sortColors(restGroupedBySuit)
    /*HandNeutral(
      trumpCardsNeutral ++ restGroupedBySuit.
        .getOrElse(Nil)
        .map(card => CardNeutral(First, card.height))
    )*/
    restSorted.length match
      case 0 => HandNeutral(trumpCardsNeutral)
      case 1 =>
        HandNeutral(
          trumpCardsNeutral ++ restSorted.head.map(card =>
            CardNeutral(First, card.height)
          )
        )
      case 2 =>
        HandNeutral(
          trumpCardsNeutral ++ restSorted.head.map(card =>
            CardNeutral(First, card.height)
          ) ++ restSorted(1).map(card => CardNeutral(Second, card.height))
        )
      case 3 =>
        HandNeutral(
          trumpCardsNeutral ++ restSorted.head.map(card =>
            CardNeutral(First, card.height)
          ) ++ restSorted(1).map(card =>
            CardNeutral(Second, card.height)
          ) ++ restSorted(2).map(card => CardNeutral(Third, card.height))
        )
  }
}
