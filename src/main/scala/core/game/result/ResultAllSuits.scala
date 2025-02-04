package core.game.result

import core.game.cards.{Hand, Suit}

case class ResultAllSuits(hand: Hand, pointsMap: Map[Suit, Int]) {
  override def toString: String =
    s"${hand.toString};${pointsMap.toList.sortBy(_._2).reverse.map((s, i) => s"${s.getLiteral}:$i").mkString(",")}"
}

object ResultAllSuits {
  def fromString(s: String): ResultAllSuits = {
    // todo: careful not safe
    val (cardsS, pointsS) = (s.split(";").head, s.split(";").tail.head)
    val pointsMap = pointsS
      .split(",")
      .map(_.split(":"))
      .map(s => Suit.fromString(s.head) -> s.tail.head.toInt)
      .toMap
    ResultAllSuits(Hand.fromString(cardsS), pointsMap)
  }
}
