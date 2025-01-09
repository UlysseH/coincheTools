package core.game.cards

import cats.effect.IO
import core.game.Player
import core.game.Player._

case class HandMap(
    cards1: List[Card],
    cards2: List[Card],
    cards3: List[Card],
    cards4: List[Card]
) {
  def initial: Boolean =
    (cards1.length + cards2.length + cards3.length + cards4.length) == 32
  def toList: List[(Player, List[Card])] = List(
    (player1, cards1),
    (player2, cards2),
    (player3, cards3),
    (player4, cards4)
  )
  def getPlayerCards(player: Player): List[Card] = player match
    case Player.player1 => cards1
    case Player.player2 => cards2
    case Player.player3 => cards3
    case Player.player4 => cards4

  def removeCard(card: Card, player: Player): HandMap = player match
    case Player.player1 => this.copy(cards1 = cards1.filterNot(_.==(card)))
    case Player.player2 => this.copy(cards2 = cards2.filterNot(_.==(card)))
    case Player.player3 => this.copy(cards3 = cards3.filterNot(_.==(card)))
    case Player.player4 => this.copy(cards4 = cards4.filterNot(_.==(card)))

  def printInfo(trumpSuit: Suit): IO[Unit] =
    IO.println(s"${if (initial) "[Initial]" else "[Playing]"} players/cards") *>
      IO.println(
        toList
          .map((s, l) =>
            (
              s,
              Hand(
                l
                  .sortBy(card =>
                    if (card.suit == trumpSuit) card.height.getTrumpRank + 100
                    else card.height.getBaseRank
                  )
                  .reverse
              )
            )
          )
          .map((p, h) =>
            s"$p : ${h.cards.map(_.getNotation).mkString(",")} (${h
                .countPoints(Suit.Spades)}pts)"
          )
          .mkString("\n")
      )
}

object HandMap {
  // TODO: this is obviously not safe, find corrections
  def fromMap(m: Map[Player, List[Card]]): HandMap =
    HandMap(m(player1), m(player2), m(player3), m(player4))
  def fromDraw(draw: Draw): HandMap =
    HandMap(draw.h1.cards, draw.h2.cards, draw.h3.cards, draw.h4.cards)
}
