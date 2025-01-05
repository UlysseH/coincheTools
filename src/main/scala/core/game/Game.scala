package core.game

import core.game.cards.{Card, Deck, Hand, Suit}
import core.game.roundTree.RandomGameGenerator

case class Game(
    players: (String, String, String, String),
    trumpSuit: Suit,
    handMap: Map[String, Hand]
) {
  def nextPlayer(name: String): String = name match
    case players._1 => players._2
    case players._2 => players._3
    case players._3 => players._4
    case players._4 => players._1

  def partner(name: String): String = name match
    case players._1 => players._3
    case players._2 => players._4
    case players._3 => players._1
    case players._4 => players._2

  def randomOptimizeRec(precision: Int) = {
    val res = List.fill(precision)(
      RandomGameGenerator.computeRec(
        this,
        players._1,
        List.empty[(Card, String)]
      )
    )
    
  }

  // val isPartner(a, b)
}
