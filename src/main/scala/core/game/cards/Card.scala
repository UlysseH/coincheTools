package core.game.cards

case class Card(suit: Suit, height: Height) {

  import Card._

  def getNotation: String = s"${height.getLiteral}${suit.getLiteral}"
}
