package core.game.cards

case class Card(suit: Suit, height: Height) {

  import Card._

  def getNotation: String = s"${height.getLiteral}${suit.getLiteral}"
}

object Card {
  def fromLitteral(s: String) = Card(
    Suit.fromString(s(1).toString),
    Height.fromString(s(0).toString)
  )
}
