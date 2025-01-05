package core.game.roundTree

import core.game.cards.{Card, Suit}

case class Round(cardsPlayed: List[Card], trumpSuit: Suit) {
  def isNextCardLegal(card: Card) = {
    cardsPlayed.length match
      case 0 => true
      case 4 => false
      case _ => {
        val isTrump = cardsPlayed.head.suit == trumpSuit
        
      }
  }
  
  
}
