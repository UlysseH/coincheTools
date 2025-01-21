package core.game

import core.game.Player
import core.game.cards.{Card, HandMap, Suit}

import scala.util.Random

case class GameState(
    id: String,
    step: Int,
    currentPlayer: Player,
    remainingHandMap: HandMap,
    askedSuit: Suit,
    trumpSuit: Suit,
    masterPlayer: Player,
    masterCard: Card
) {
  private val playerCards: List[Card] =
    remainingHandMap.getPlayerCards(currentPlayer)

  private val playerAskedColorCards: List[Card] =
    playerCards.filter(_.suit.==(askedSuit))
  private val playerHasAskedColorCard: Boolean = playerAskedColorCards.nonEmpty

  private val playerTrumpCards: List[Card] =
    playerCards.filter(_.suit.==(trumpSuit))
  private val playerHasTrumpCard: Boolean = playerTrumpCards.nonEmpty
  private val isTrumpRound: Boolean = askedSuit == trumpSuit

  def isPlayable(card: Card): Option[(List[Card], Boolean)] =
    (
      card,
      isTrumpRound,
      playerHasAskedColorCard,
      playerHasTrumpCard,
      masterPlayer,
      masterCard
    ) match {
      case (card, false, _, _, _, _) if card.suit == askedSuit =>
        Some((Nil, card.isAbove(masterCard, trumpSuit)))
      case (_, false, true, _, _, _) => None
      case (_, false, false, _, _, _)
          if masterPlayer == currentPlayer.partner =>
        Some((askedSuit.generateCards, false))
      case (_, false, false, false, _, _) =>
        Some((askedSuit.generateCards ++ trumpSuit.generateCards, false))
      case (card, false, false, true, _, masterCard)
          if card.isAbove(masterCard, trumpSuit) =>
        Some((askedSuit.generateCards, true))
      case (_, false, false, true, _, masterCard) =>
        Some(
          (
            askedSuit.generateCards ++ trumpSuit.generateCards.filterNot(
              _.isAbove(masterCard, trumpSuit)
            ),
            false
          )
        )
      case (_, true, _, false, _, _) => Some((trumpSuit.generateCards, false))
      case (card, true, _, true, _, _) if card.suit != trumpSuit => None
      case (card, true, _, true, _, masterCard)
          if card.isAbove(masterCard, trumpSuit) =>
        Some((Nil, true))
      case (_, true, _, true, _, masterCard) =>
        Some(
          (
            trumpSuit.generateCards.filterNot(_.isAbove(masterCard, trumpSuit)),
            false
          )
        )
    }

  def generateRandomPlayableCardWithCorrelated: (List[Card], Boolean) =
    // unsafe but there should ALWAYS be a playable card if isPlayable is correct
    Random.shuffle(playerCards).flatMap(isPlayable).head
}
