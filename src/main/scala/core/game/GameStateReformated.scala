package core.game

import core.game.cards.{Card, HandMap, Height, Suit}

case class GameStateReformated(
    id: String,
    // Trump Suit and strongest card for now
    trumpSuitState: (Suit, Int),
    step: Int,
    currentPlayer: Player,
    playedCards: List[Card],
    masterPlayer: Option[Player],
    handMap: HandMap,
    forbiddenCardsMap: Map[Player, Card]
) {
  val trumpSuit = trumpSuitState._1
  val isTrumpTrick: Boolean = playedCards.head.suit.==(trumpSuit)
  val playerCards: List[Card] = handMap.toMap(currentPlayer)
  val partnerIsMaster = currentPlayer.partner == masterPlayer

  /** If player plays a trump card, it must be above the best already played
    * trump card if player can, else any under will do Also returns the
    * correlated excluded cards for futur handMap generation in the case of
    * random generation of game trees
    */
  def playableTrumpCardsAndNegativeCorrelation: (List[Card], List[Card]) = {
    val minTrumpRank: Int = playedCards
      .filter(_.suit.==(trumpSuit))
      .map(_.height.getTrumpRank)
      .sorted
      .reverse
      .headOption
      .getOrElse(0)
    val playerTrumpCards = playerCards.filter(_.suit.==(trumpSuit))

    val playerAboveMinTrumpCards =
      playerTrumpCards.filter(_.height.getTrumpRank.>=(minTrumpRank))

    (playerAboveMinTrumpCards, playerTrumpCards) match
      // If player has to trump card, he cannot get one in future generations
      case (Nil, Nil) =>
        (playerCards, Height.values.toList.map(h => Card(trumpSuit, h)))
      // For future generation, current player can no longer have above Trump Card
      case (Nil, _) =>
        (
          playerTrumpCards,
          Height.values.toList
            .filter(_.getTrumpRank.>(minTrumpRank))
            .map(h => Card(trumpSuit, h))
        )
      case _ => (playerAboveMinTrumpCards, Nil)
  }

  def pissCards: List[Card] = playerCards
    .filterNot(_.suit.==(trumpSuit))
    .filterNot(
      _.suit.==(playedCards.headOption.map(_.suit).getOrElse(Suit.None))
    )

  def rightColorCards: List[Card] = playedCards.headOption match
    case Some(value) => playerCards.filter(_.suit.==(value.suit))
    case None        => playerCards

  /*val res = if (isTrumpTrick) playableTrumpCardsAndNegativeCorrelation else {
    if (rightColorCards.nonEmpty) (rightColorCards, playerCards) else {
      if (partnerIsMaster) {
        pla
      }
    }
  } */

  //def cardIsAboveBestTrump =

  val playerHasTrumpCards: Boolean = playedCards.exists(_.suit.==(trumpSuit))

  val playerHasAboveMinTrumpCards: Boolean =
    playerCards
      .filter(_.suit.==(trumpSuit))
      .exists(_.height.getTrumpRank.>=(trumpSuitState._2))

  /*def out(card: Card) =
    (
      isTrumpTrick,
      playedCards.head.suit,
      card.suit,
      card.height.getTrumpRank,
      partnerIsMaster
    ) match {
      case (true, _, cardSuit, rank, _)
          if cardSuit == trumpSuit && rank > trumpSuitState._2 =>
        (true, Nil)
      case (true, _, cardSuit, _, _)
          if cardSuit == trumpSuit && !playerHasAboveMinTrumpCards =>
        (
          true,
          Height.values.toList
            .filterNot(_.getTrumpRank.>=(trumpSuitState._2))
            .map(h => Card(trumpSuit, h))
        )
      case (true, _, cardSuit, _, _) if cardSuit == trumpSuit => (false, Nil)
      case (false, askedSuit, cardSuit, _, _) if cardSuit == askedSuit =>
        (true, Nil)
      case (false, _, suit, _, false) if suit != trumpSuit && !playerHasTrumpCards => (true, Nil)
      case (false, _, suit, _, false) if suit != trumpSuit => (false, Nil)
        )
      case (false )
    }*/

  /*def isCardPlayableWithNegativeCorrelation(card: Card): (Boolean, List[Card]) =
    if (isTrumpTrick) {
      if (card.suit.==(trumpSuit)) {
        if (card.height.getTrumpRank.>(trumpSuitState._2)) (true, Nil)
        else
          (
            true,
            Height.values.toList
              .filterNot(_.getTrumpRank.>(trumpSuitState._2))
              .map(h => Card(trumpSuit, h))
          )
      } else (true)
    }*/

  // val playableCardsFilter =
}
