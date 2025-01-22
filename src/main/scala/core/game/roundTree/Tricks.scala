package core.game.roundTree

import core.game.Player
import core.game.Player._
import core.game.cards.{Card, Height, Suit}

case class Tricks(
    wonBy: Player,
    points: Int,
    cards: CardsPlayed,
    firstPlayer: Player
) {
  def print =
    s"[wonBy] $wonBy | [points] $points | ${cardsAsList.map(_.getNotation).mkString(",")} | [firstPlayer] $firstPlayer"

  def isTrumpTrick = cards.trumpSuit == cards.first._1.suit

  def cardsAsList: List[Card] =
    List(cards.first._1, cards.second._1, cards.third._1, cards.fourth._1)

  def forbiddenHandMap: Map[Player, List[Card]] = {
    def forbiddenAtTrumpSuit(card: Card): List[Card] = if (
      isTrumpTrick && card.suit.==(
        cards.first._1.suit
      ) && card.height.getTrumpRank
        .<(
          cards.first._1.height.getTrumpRank
        )
    ) card.generateAboveTrumpCards
    else Nil

    def forbiddenAtSuit(card: Card): List[Card] = if (
      !isTrumpTrick && !card.suit.==(cards.first._1.suit)
    ) Height.values.map(h => Card(cards.first._1.suit, h)).toList
    else Nil

    // hasn't played trump card and partner is not master
    def forbiddenOther(player: Player, card: Card): List[Card] = if (
      isTrumpTrick && !card.suit.==(cards.first._1.suit) && cards
        .partnerIsMaster(player)
    )
      Height.values.map(h => Card(cards.first._1.suit, h)).toList
    else Nil

    // val trumpTrick = cards.head.suit.==(trumpSuit)
    val cardHand = Map(
      cards.first._2 -> cards.first._1,
      cards.second._2 -> cards.second._1,
      cards.third._2 -> cards.third._1,
      cards.fourth._2 -> cards.fourth._1
    )

    val res = cardHand.map((player, card) =>
      (
        player -> (forbiddenAtSuit(card) ++ forbiddenOther(
          player,
          card
        ) ++ forbiddenAtTrumpSuit(card)).distinct
      )
    )

    res
  }
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
      CardsPlayed(
        playedCards.head,
        playedCards(1),
        playedCards(2),
        playedCards(3),
        trumpSuit
      ),
      playedCards.head._2
    )
  }

  def computePoints(tricksList: List[Tricks], player: Player): Int = {
    tricksList
      .filter(t => (t.wonBy == player) || (t.wonBy == player.partner))
      .map(_.points)
      .sum
  }
}

case class CardsPlayed(
    first: (Card, Player),
    second: (Card, Player),
    third: (Card, Player),
    fourth: (Card, Player),
    trumpSuit: Suit
) {
  def partnerIsMaster(player: Player): Boolean = player match {
    case third._2 if first._1.isAbove(second._1, trumpSuit) => true
    case fourth._2
        if second._1.isAbove(
          third._1,
          trumpSuit
        ) && second._1.isAbove(first._1, trumpSuit) =>
      true
    case _ => false
  }
}
