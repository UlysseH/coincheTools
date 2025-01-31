package core.game.cards.neutral

import core.game.cards.neutral.SuitNeutral.{First, Trump}
import core.game.cards.{Card, Hand, Suit}

import scala.annotation.tailrec

case class HandNeutral(cards: List[CardNeutral]) {}

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

  def fromHand(hand: Hand, trumpSuit: Suit) = {
    val cards = hand.cards

    val trumpCardsNeutral = cards
      .filter(_.suit.==(trumpSuit))
      .map(card => CardNeutral(Trump, card.height))
    val restGroupedBySuit = cards
      .filterNot(_.suit.==(trumpSuit))
      .groupBy(_.suit)
      .toList
      .map((_, cards) => cards.sortBy(_.height.getBaseRank).reverse)
    val restSorted = sortColors(restGroupedBySuit)
    HandNeutral(
      trumpCardsNeutral ++ restGroupedBySuit.
        .getOrElse(Nil)
        .map(card => CardNeutral(First, card.height))
    )
  }
}
