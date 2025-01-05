package core.game.cards

import scala.util.Random

object Deck {
  private val orderedList: List[Card] = Suit.values.toList
    .filterNot(_.==(Suit.None))
    .flatMap(suit => Height.values.map(height => Card(suit, height)))

  def shuffled: Deck = Deck(
    Random
      .shuffle(orderedList)
  )

}

case class Deck(cards: List[Card]) {
  def cut: Deck = {
    val pivot = Random.between(1, 31)
    val split = cards.splitAt(pivot)
    Deck(split._2 ++ split._1)
  }
  def distribute(ptn: DistributePattern): Draw = ptn match
    case DistributePattern.ThreeThreeTwo =>
      Draw(
        Hand(cards.slice(0, 3) ++ cards.slice(12, 15) ++ cards.slice(24, 26)),
        Hand(cards.slice(3, 6) ++ cards.slice(15, 18) ++ cards.slice(26, 28)),
        Hand(cards.slice(6, 9) ++ cards.slice(18, 21) ++ cards.slice(28, 30)),
        Hand(cards.slice(9, 12) ++ cards.slice(21, 24) ++ cards.slice(30, 32))
      )
    case DistributePattern.ThreeTwoThree =>
      Draw(
        Hand(cards.slice(0, 3) ++ cards.slice(12, 14) ++ cards.slice(20, 23)),
        Hand(cards.slice(3, 6) ++ cards.slice(14, 16) ++ cards.slice(23, 26)),
        Hand(cards.slice(6, 9) ++ cards.slice(16, 18) ++ cards.slice(26, 29)),
        Hand(cards.slice(9, 12) ++ cards.slice(18, 20) ++ cards.slice(29, 32))
      )
    case DistributePattern.TwoThreeThree =>
      Draw(
        Hand(cards.slice(0, 2) ++ cards.slice(8, 11) ++ cards.slice(20, 23)),
        Hand(cards.slice(2, 4) ++ cards.slice(11, 14) ++ cards.slice(23, 26)),
        Hand(cards.slice(4, 6) ++ cards.slice(14, 17) ++ cards.slice(26, 29)),
        Hand(cards.slice(6, 8) ++ cards.slice(17, 20) ++ cards.slice(29, 32))
      )
    case DistributePattern.FourFour =>
      Draw(
        Hand(cards.slice(0, 4) ++ cards.slice(16, 20)),
        Hand(cards.slice(4, 8) ++ cards.slice(20, 24)),
        Hand(cards.slice(8, 12) ++ cards.slice(24, 28)),
        Hand(cards.slice(12, 16) ++ cards.slice(28, 32))
      )
}
