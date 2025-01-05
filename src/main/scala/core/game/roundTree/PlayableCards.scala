package core.game.roundTree

import scala.util.Random
import core.game.Game
import core.game.cards.{Card, Suit}

case class PlayableCards(
    game: Game,
    currentPlayer: String,
    playerCards: List[Card],
    cardsPlayed: List[(Card, String)],
    trumpSuit: Suit,
    currentSuit: Suit
) {
  private val partnerIsMaster: Boolean =
    cardsPlayed.headOption.exists((_, player) =>
      game.partner(player) == currentPlayer
    )
  private val playerTrumpCards = playerCards.filter(_.suit.==(trumpSuit))

  private val playableTrumpCards: List[Card] =
    if (cardsPlayed.isEmpty) playerTrumpCards
    else if (cardsPlayed.headOption.exists(_._1.suit.!=(trumpSuit)))
      playerTrumpCards
    else {
      val bestTrumpRank = cardsPlayed.head._1.height.getTrumpRank
      val aboveCards = playerCards
        .filter(_.suit.==(trumpSuit))
        .filter(_.height.getTrumpRank.>(bestTrumpRank))
      if (aboveCards.nonEmpty) aboveCards
      else playerTrumpCards
    }

  private val rightColorCards: List[Card] =
    playerCards.filter(_.suit.==(currentSuit))

  private val pissCards: List[Card] = {
    playerCards.filter(elt =>
      (elt.suit != currentSuit) && (elt.suit != trumpSuit)
    )
  }

  val cards: List[Card] = cardsPlayed.length match
    case 0 => playerCards
    case _ if currentSuit == trumpSuit =>
      if (playableTrumpCards.nonEmpty) playableTrumpCards
      else pissCards // .take(1)
    case _ if rightColorCards.nonEmpty    => rightColorCards
    case _ if partnerIsMaster             => playableTrumpCards ++ pissCards
    case _ if playableTrumpCards.nonEmpty => playableTrumpCards
    case _                                => pissCards // .take(1)

  def generateRandom: Card = cards(Random.nextInt(cards.length))
  def generateRandomWithRest: (Card, List[Card]) = {
    val randomCard = cards(Random.nextInt(cards.length))
    (randomCard, playerCards.filterNot(_.==(randomCard)))
  }

  def generateRandomN(n: Int): List[Card] = Random.shuffle(cards).take(n)
}
