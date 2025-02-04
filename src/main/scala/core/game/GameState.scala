package core.game

import core.game.Player
import core.game.cards.{Card, HandMap, Suit}

import scala.util.Random

case class GameState(
    gameId: String,
    step: Int,
    playedCardsWithPlayers: List[(Card, Player)],
    currentPlayer: Player,
    remainingHandMap: HandMap,
    askedSuit: Suit,
    trumpSuit: Suit,
    masterPlayer: Option[Player],
    masterCard: Option[Card]
) {
  val isFinal: Boolean =
    remainingHandMap.toMap.toList.map(_._2.length).sum.==(0)

  private val playerCards: List[Card] =
    remainingHandMap.getPlayerCards(currentPlayer)

  private val playerAskedColorCards: List[Card] =
    playerCards.filter(_.suit.==(askedSuit))
  private val playerHasAskedColorCard: Boolean = playerAskedColorCards.nonEmpty

  private val playerTrumpCards: List[Card] =
    playerCards.filter(_.suit.==(trumpSuit))
  private val playerHasTrumpCard: Boolean = playerTrumpCards.nonEmpty
  private val isTrumpRound: Boolean = askedSuit == trumpSuit

  def computeNextGameState(card: Card, takesLead: Boolean): GameState = {
    // For when the trick is over since 4 cards have been played
    if (step % 4 == 0)
      copy(
        step = step + 1,
        playedCardsWithPlayers =
          playedCardsWithPlayers.appended((card, currentPlayer)),
        currentPlayer =
          if (takesLead) currentPlayer
          else masterPlayer.get,
        remainingHandMap = remainingHandMap.removeCard(card, currentPlayer),
        askedSuit = Suit.None,
        masterPlayer = Some(currentPlayer),
        masterCard = Some(card)
      )
    else
      // Trick is not complete
      copy(
        step = step + 1,
        playedCardsWithPlayers =
          playedCardsWithPlayers.appended((card, currentPlayer)),
        currentPlayer = currentPlayer.nextPlayer,
        remainingHandMap = remainingHandMap.removeCard(card, currentPlayer),
        askedSuit =
          if (askedSuit == Suit.None)
            card.suit
          else askedSuit,
        masterPlayer =
          if (takesLead) Some(currentPlayer)
          else masterPlayer,
        masterCard =
          if (takesLead) Some(card)
          else masterCard
      )
  }

  // TODO write a "fast" version without the negative correlation and compare execution time
  def isPlayable(card: Card): Option[(Card, List[Card], Boolean)] = {
    if (askedSuit == Suit.None) Some((card, Nil, true))
    else
      // println(s"master ${masterPlayer} with ${masterCard.get.toString}")
      (
        card,
        isTrumpRound,
        playerHasAskedColorCard,
        playerHasTrumpCard,
        masterPlayer,
        masterCard
      ) match {
        case (card, false, _, _, _, _) if card.suit == askedSuit =>
          // println(askedSuit)
          Some((card, Nil, card.isAbove(masterCard.get, trumpSuit)))
        case (_, false, true, _, _, _) => None
        case (_, false, false, _, Some(masterPlayer), _)
            if masterPlayer == currentPlayer.partner =>
          Some((card, askedSuit.generateCards, false))
        case (_, false, false, false, _, _) =>
          Some(
            (card, askedSuit.generateCards ++ trumpSuit.generateCards, false)
          )
        case (card, false, false, true, _, Some(masterCard))
            if card.suit
              .==(trumpSuit) && card.isAbove(masterCard, trumpSuit) =>
          // println("Hey Yo !")
          Some((card, askedSuit.generateCards, true))
        case (_, false, false, true, _, Some(masterCard))
            if card.suit
              .==(trumpSuit) =>
          // println("Hey Ya !")
          Some(
            (
              card,
              askedSuit.generateCards ++ trumpSuit.generateCards.filter(
                _.isAbove(masterCard, trumpSuit)
              ),
              false
            )
          )
        case (_, false, false, true, _, _) => None
        case (_, true, _, false, _, _) =>
          Some((card, trumpSuit.generateCards, false))
        case (card, true, _, true, _, _) if card.suit != trumpSuit => None
        case (card, true, _, true, _, Some(masterCard))
            if card.isAbove(masterCard, trumpSuit) =>
          Some((card, Nil, true))
        case (_, true, _, true, _, Some(masterCard))
            if playerTrumpCards.exists(_.isAbove(masterCard, trumpSuit)) =>
          None
        case (_, true, _, true, _, masterCard) =>
          Some(
            (
              card,
              trumpSuit.generateCards.filter(
                _.isAbove(masterCard.get, trumpSuit)
              ),
              false
            )
          )
      }
  }

  // TODO : investigate to understand if or not this is done lazily
  def generateRandomPlayableCardWithCorrelated: (Card, List[Card], Boolean) =
    // unsafe but there should ALWAYS be a playable card if isPlayable is correct
    Random.shuffle(playerCards).flatMap(isPlayable).head

  // This version is by no means lazy ;)
  def generatePlayableCards: List[(Card, List[Card], Boolean)] =
    playerCards.flatMap(isPlayable)
}
