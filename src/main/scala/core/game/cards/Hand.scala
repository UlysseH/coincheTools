package core.game.cards

case class Hand(cards: List[Card]) {
  def remove(card: Card): Hand = Hand(cards.filterNot(_.==(card)))
  def getNotation: List[String] = cards.map(_.getNotation)
  def getNotationBaseOrder: List[String] =
    cards
      .groupBy(_.suit)
      .toList
      .flatMap(_._2.sortBy(_.height.getBaseRank).map(_.getNotation).reverse)

  def getNotationTrumpOrder: List[String] =
    cards
      .groupBy(_.suit)
      .toList
      .flatMap(_._2.sortBy(_.height.getTrumpRank).map(_.getNotation).reverse)

  def toStringTrumpOrdered(trumpSuit: Suit) = cards
    .sortBy(card =>
      if (card.suit == trumpSuit) card.height.getTrumpRank + 100
      else card.height.getBaseRank
    )
    .reverse
    .map(_.getNotation)

  // Plusieures valeurs possibles bien entendu
  def isMisery: Boolean = (cards.map(_.height.getBaseRank).sum < 11)

  def countPoints(trumpSuit: Suit): Int = cards.map {
    case Card(suit, height) if suit == trumpSuit => height.getTrumpPoints
    case Card(_, height)                         => height.getBasePoints
  }.sum
}
