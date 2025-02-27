package core.game.cards

case class Hand(cards: List[Card]) {
  override def toString: String = cards.map(_.toString).mkString(",")
  def remove(card: Card): Hand = Hand(cards.filterNot(_.==(card)))
  def toStringBaseOrder: List[String] =
    cards
      .groupBy(_.suit)
      .toList
      .flatMap(_._2.sortBy(_.height.getBaseRank).map(_.toString).reverse)

  def toStringTrumpOrder: List[String] =
    cards
      .groupBy(_.suit)
      .toList
      .flatMap(_._2.sortBy(_.height.getTrumpRank).map(_.toString).reverse)

  def toStringTrumpOrdered(trumpSuit: Suit): List[String] = cards
    .sortBy(card =>
      if (card.suit == trumpSuit) card.height.getTrumpRank + 100
      else card.height.getBaseRank
    )
    .reverse
    .map(_.toString)

  // def toNeutralHand

  // Plusieures valeurs possibles bien entendu
  def isMisery: Boolean = (cards.map(_.height.getBaseRank).sum < 11)

  def countPoints(trumpSuit: Suit): Int = cards.map {
    case Card(suit, height) if suit == trumpSuit => height.getTrumpPoints
    case Card(_, height)                         => height.getBasePoints
  }.sum

  // TODO make a generic version for generating from any osition
  def generateRandomHandMap: HandMap = {
    val shuffledDeck = Deck.shuffled
    val filteredDeck =
      shuffledDeck.cards.filterNot(card => cards.contains(card))
    HandMap(
      this.cards,
      filteredDeck.slice(0, 8),
      filteredDeck.slice(8, 16),
      filteredDeck.slice(16, 24)
    )
  }
}

object Hand {
  def fromString(s: String): Hand =
    Hand(s.split(",").map(Card.fromLitteral).toList)

  /** (Js, Xs, Xs, Ad, Th, Xh, Xx, Xx), Xs <
    */
}
