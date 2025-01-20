package core.game.cards

import cats.effect.IO
import core.game.{Game, Player, Result}
import core.game.Player.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}

import scala.util.Random

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
  def toMap: Map[Player, List[Card]] = toList.map((p, c) => p -> c).toMap
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

  def toStringMap: Map[String, String] = Map(
    "cards1" -> cards1.map(_.getNotation).mkString(","),
    "cards2" -> cards2.map(_.getNotation).mkString(","),
    "cards3" -> cards3.map(_.getNotation).mkString(","),
    "cards4" -> cards4.map(_.getNotation).mkString(",")
  )

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

  def randomExceptPlayer(
      currentPlayer: Player,
      forbiddenMap: Map[Player, List[Card]]
  ): HandMap = {
    val handMap = toMap
    val shuffledRest =
      Random.shuffle(
        handMap.filterNot(_._1.==(currentPlayer)).flatMap(_._2)
      )
    val numCardMap = handMap.toList
      .filterNot(_._1.==(currentPlayer))
      .map((player, l) => (player, l.length))
      .scanLeft((EMPTY, 0, 0))((acc, x) => (x._1, acc._3, acc._3 + x._2))
      .tail

    val listOfDecksToChooseFrom =
      numCardMap.scanLeft(shuffledRest)((deck, elt) =>
        deck.filterNot(card =>
          forbiddenMap(elt._1).contains(card)
        ) ++ forbiddenMap(elt._1)
      )

    val x =
      numCardMap.scanLeft((shuffledRest, List.empty[Card]))((acc, elt) => {
        val player = elt._1
        val rest = acc._1
        val restForbiddenCardsRearranged = (rest.filterNot(card =>
          forbiddenMap(player).contains(card)
        ) ++ forbiddenMap(player)).toList
        (
          restForbiddenCardsRearranged
            .slice(8, restForbiddenCardsRearranged.length),
          restForbiddenCardsRearranged.slice(0, 8)
        )
      })

    val newHandMap =
      numCardMap
        .map((player, x, y) => {
          player -> shuffledRest.slice(x, y)
        })
        .toMap
        .+(currentPlayer -> handMap(currentPlayer))
    HandMap(
      newHandMap(player1).toList,
      newHandMap(player2).toList,
      newHandMap(player3).toList,
      newHandMap(player4).toList
    )
  }

  def getBestExpectedResultTeamA(precision: Int): Result = {
    val games = List(Spades, Diamonds, Clubs, Hearts).map(suit =>
      Game.fromHandMap(this, suit)
    )
    val bestGame =
      games
        .flatMap(_.generateRandomOptGame(precision))
        .map(_._1)
        .maxBy(_.pointsA)
    bestGame
  }

  def optimizeValuesForPlayerHand(
      player: Player,
      numSamples: Int,
      precision: Int,
      forbiddenHandMap: Map[Player, List[Card]]
  ): List[Result] = {
    val samples =
      List.fill(numSamples)(randomExceptPlayer(player, forbiddenHandMap))
    val games = samples.flatMap(hMap =>
      List(Spades, Diamonds, Clubs, Hearts).map(suit =>
        Game.fromHandMap(hMap, suit)
      )
    )
    val results = games.flatMap(_.generateRandomOptGame(precision)).map(_._1)

    results
  }
}

object HandMap {
  // TODO: this is obviously not safe, find corrections
  def fromMap(m: Map[Player, List[Card]]): HandMap =
    HandMap(m(player1), m(player2), m(player3), m(player4))
  def fromDraw(draw: Draw): HandMap =
    HandMap(draw.h1.cards, draw.h2.cards, draw.h3.cards, draw.h4.cards)
  def fromStrings(s1: String, s2: String, s3: String, s4: String): HandMap =
    HandMap(
      s1.split(",").toList.map(s => Card.fromLitteral(s)),
      s2.split(",").toList.map(s => Card.fromLitteral(s)),
      s3.split(",").toList.map(s => Card.fromLitteral(s)),
      s4.split(",").toList.map(s => Card.fromLitteral(s))
    )

  def randomHand: HandMap = {
    val shuffledDeck = Deck.shuffled
    HandMap(
      shuffledDeck.cards.slice(0, 8),
      shuffledDeck.cards.slice(8, 16),
      shuffledDeck.cards.slice(16, 24),
      shuffledDeck.cards.slice(24, 32)
    )
  }
}
