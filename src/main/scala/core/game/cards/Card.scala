package core.game.cards

case class Card(suit: Suit, height: Height) {

  import Card._

  override def toString: String = s"${height.getLiteral}${suit.getLiteral}"

  def generateAboveTrumpCards: List[Card] = Height.values.toList
    .filter(_.getTrumpRank.>(height.getTrumpRank))
    .map(h => Card(suit, height))

  def isAbove(card: Card, trumpSuit: Suit): Boolean = {
    /*if (suit == trumpSuit)
      height.getTrumpRank > card.height.getTrumpRank
    else height.getBaseRank > card.height.getBaseRank*/
    if (suit == trumpSuit) {
      if (card.suit != trumpSuit) true
      else height.getTrumpRank > card.height.getTrumpRank
    } else {
      if (card.suit == trumpSuit) false
      else height.getBaseRank > card.height.getBaseRank
    }
  }
}

object Card {
  def fromLitteral(s: String) = Card(
    Suit.fromString(s(1).toString),
    Height.fromString(s(0).toString)
  )
}
