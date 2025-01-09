package core.game

import cats.effect.IO
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import core.game.cards.{Card, Deck, DistributePattern, Hand, Suit}
import core.game.roundTree.Tricks
import io.circe.syntax._

import scala.annotation.tailrec
import scala.util.Random

case class Game(
    players: (String, String, String, String),
    currentPlayer: String,
    trumpSuit: Suit,
    handMap: Map[String, Hand],
    cardsInPlay: List[(Card, String)],
    tricksList: List[Tricks],
    cardsFromBeginning: List[Card],
    step: Int,
    isComplete: Boolean
) {
  def printInfo: IO[Unit] = Game.printInfo(handMap, trumpSuit)

  // def

  /** Playable Cards functionalities
    */
  val currentSuit: Suit =
    cardsInPlay.headOption.map(_._1.suit).getOrElse(Suit.None)

  val playerCards: List[Card] = handMap(currentPlayer).cards

  val playableCards: List[Card] = {
    val partnerIsMaster: Boolean =
      cardsInPlay.headOption.exists((_, player) =>
        partner(player) == currentPlayer
      )
    val playerTrumpCards = playerCards.filter(_.suit.==(trumpSuit))

    val playableTrumpCards: List[Card] =
      if (cardsInPlay.isEmpty) playerTrumpCards
      else if (cardsInPlay.headOption.exists(_._1.suit.!=(trumpSuit)))
        playerTrumpCards
      else {
        val bestTrumpRank = cardsInPlay.head._1.height.getTrumpRank
        val aboveCards = playerCards
          .filter(_.suit.==(trumpSuit))
          .filter(_.height.getTrumpRank.>(bestTrumpRank))
        if (aboveCards.nonEmpty) aboveCards
        else playerTrumpCards
      }

    val rightColorCards: List[Card] =
      playerCards.filter(_.suit.==(currentSuit))

    val pissCards: List[Card] = {
      playerCards.filter(elt =>
        (elt.suit != currentSuit) && (elt.suit != trumpSuit)
      )
    }

    cardsInPlay.length match
      case 0 => playerCards
      case _ if currentSuit == trumpSuit =>
        if (playableTrumpCards.nonEmpty) playableTrumpCards
        else pissCards // .take(1)
      case _ if rightColorCards.nonEmpty    => rightColorCards
      case _ if partnerIsMaster             => playableTrumpCards ++ pissCards
      case _ if playableTrumpCards.nonEmpty => playableTrumpCards
      case _                                => pissCards // .take(1)
  }

  def nextStep(cardPlayed: Card): Game = {
    val newHandMap = handMap.updated(
      currentPlayer,
      handMap(currentPlayer).remove(cardPlayed)
    )

    cardsInPlay.length match
      case 3 => {
        val newTricks = tricksList.appended(
          Tricks.fromCards(
            cardsInPlay.appended((cardPlayed, currentPlayer)),
            trumpSuit,
            tricksList.length == 7
          )
        )
        this.copy(
          currentPlayer = newTricks.last.wonBy,
          handMap = newHandMap,
          cardsInPlay = List.empty[(Card, String)],
          newTricks,
          cardsFromBeginning = cardsFromBeginning.appended(cardPlayed),
          step = step + 1,
          tricksList.length == 7
        )
      }
      case _ =>
        this.copy(
          currentPlayer = nextPlayer(currentPlayer),
          handMap = newHandMap,
          cardsInPlay = cardsInPlay.appended((cardPlayed, currentPlayer)),
          cardsFromBeginning = cardsFromBeginning.appended(cardPlayed),
          step = step + 1
        )
  }

  def printTricks: IO[Unit] = IO.println("\n[info] Tricks") *> IO.println(
    tricksList.map(_.print).mkString("\n")
  )

  /** Points
    */
  def computePoints: Map[String, Int] = {
    val pointsA = tricksList
      .filter(t => (t.wonBy == players._1) || (t.wonBy == players._3))
      .map(_.points)
      .sum
    val pointsB = tricksList
      .filter(t => (t.wonBy == players._2) || (t.wonBy == players._4))
      .map(_.points)
      .sum
    Map(
      players._1 -> pointsA,
      players._2 -> pointsB,
      players._3 -> pointsA,
      players._4 -> pointsB
    )
  }

  def printPoints: IO[Unit] = IO.println(
    s"\n[points] Team 1: ${computePoints(players._1)} | Team 2: ${computePoints(players._2)}"
  )

  /** Players functions
    */
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

  // def leadingPlayer: String =

  def generateRandomPlayableCard: Card = playableCards(
    Random.nextInt(playableCards.length)
  )

  def generateRandomPlayableCardWithRest: (Card, List[Card]) = {
    val randomCard = playableCards(Random.nextInt(playableCards.length))
    (randomCard, playerCards.filterNot(_.==(randomCard)))
  }

  def generateRandomPlayableCardN(n: Int): List[Card] =
    Random.shuffle(playableCards).take(n)

  /** Random Game Generator
    */
  @tailrec
  final def generateRandomGameFromHere: Game = if (isComplete) this
  else
    nextStep(
      generateRandomPlayableCard
    ).generateRandomGameFromHere

  /** Generate a new game with random hand map for players other than current
    * player
    */
  def generateRandomAdjacentHandMapFromHere: Game = {
    val shuffledRest =
      Random.shuffle(
        handMap.toList.filterNot(_._1.==(currentPlayer)).flatMap(_._2.cards)
      )
    val numCardMap = handMap.toList
      .filterNot(_._1.==(currentPlayer))
      .map((player, l) => (player, l.cards.length))
      .scanLeft(("", 0, 0))((acc, x) => (x._1, acc._3, acc._3 + x._2))
      .tail
    val newHandMap =
      numCardMap
        .map((player, x, y) => player -> Hand(shuffledRest.slice(x, y)))
        .toMap
        .+(currentPlayer -> handMap(currentPlayer))
    this.copy(handMap = newHandMap)
  }

  def randomOptimizeRec(precision: Int): Game =
    if (isComplete) this
    else {
      val res = List.fill(precision)(
        generateRandomAdjacentHandMapFromHere.generateRandomGameFromHere
      )
      val y = res
        .groupBy(_.cardsFromBeginning(step))
        .toList
        .map((c, l) =>
          (
            c,
            l.map(_.computePoints(currentPlayer)).sum / l.length
          )
        )
        .sortBy(_._2)
        .reverse

      // println(y.mkString("\n"))
      // println("\n")

      nextStep(y.head._1).randomOptimizeRec(precision)
    }

  def generateAllTricksWithPoints(
      h1: Hand,
      h2: Hand,
      h3: Hand,
      h4: Hand,
      trumpSuit: Suit
  ): List[Tricks] = {
    h1.cards.flatMap(c1 =>
      h2.cards.flatMap(c2 =>
        h3.cards.flatMap(c3 =>
          h4.cards.flatMap(c4 =>
            val t1 = Tricks.fromCards(
              List(
                (c1, players._1),
                (c2, players._2),
                (c3, players._3),
                (c4, players._4)
              ),
              trumpSuit
            )
            val t2 = Tricks.fromCards(
              List(
                (c2, players._2),
                (c3, players._3),
                (c4, players._4),
                (c1, players._1)
              ),
              trumpSuit
            )
            val t3 = Tricks.fromCards(
              List(
                (c3, players._3),
                (c4, players._4),
                (c1, players._1),
                (c2, players._2)
              ),
              trumpSuit
            )
            val t4 = Tricks.fromCards(
              List(
                (c4, players._4),
                (c1, players._1),
                (c2, players._2),
                (c3, players._3)
              ),
              trumpSuit
            )

            List(
              Tricks(t1.wonBy, t1.points, List(c1, c2, c3, c4), players._1),
              Tricks(t2.wonBy, t2.points, List(c2, c3, c4, c1), players._2),
              Tricks(t3.wonBy, t3.points, List(c3, c4, c1, c2), players._3),
              Tricks(t4.wonBy, t4.points, List(c4, c1, c2, c3), players._4)
            )
          )
        )
      )
    )
  }

  // val isPartner(a, b)
}

object Game {
  case class OptimizedGameResult(
      players: (String, String, String, String),
      handMap: Map[String, Hand],
      trumpSuit: Suit,
      pointsA: Int,
      pointsB: Int
  ) {
    def toMap: Map[String, String] = Map(
      players._1 -> handMap(players._1)
        .toStringTrumpOrdered(trumpSuit)
        .mkString(","),
      players._2 -> handMap(players._2)
        .toStringTrumpOrdered(trumpSuit)
        .mkString(","),
      players._3 -> handMap(players._3)
        .toStringTrumpOrdered(trumpSuit)
        .mkString(","),
      players._4 -> handMap(players._4)
        .toStringTrumpOrdered(trumpSuit)
        .mkString(","),
      "trumpSuit" -> trumpSuit.getLiteral,
      "pointsA" -> pointsA.toString,
      "pointsB" -> pointsB.toString
    )
  }
  def init(
      players: (String, String, String, String),
      trumpSuit: Suit,
      deck: Deck,
      pattern: DistributePattern
  ): Game = {
    val cutDeck = deck.cut
    val draw = cutDeck.distribute(pattern)
    val handMap = Map(
      players._1 -> draw.h1,
      players._2 -> draw.h2,
      players._3 -> draw.h3,
      players._4 -> draw.h4
    )
    Game(
      players,
      players._1,
      trumpSuit,
      handMap,
      List.empty[(Card, String)],
      List.empty[Tricks],
      List.empty[Card],
      0,
      false
    )
  }

  def printInfo(handMap: Map[String, Hand], trumpSuit: Suit): IO[Unit] =
    IO.println("[info] players/cards") *>
      IO.println(
        handMap.toList
          .map((s, l) =>
            (
              s,
              Hand(
                l.cards
                  .sortBy(card =>
                    if (card.suit == trumpSuit) card.height.getTrumpRank + 100
                    else card.height.getBaseRank
                  )
                  .reverse
              )
            )
          )
          .map((p, h) =>
            s"$p : ${h.cards.map(_.getNotation).mkString(",")} (${h.countPoints(Suit.Spades)}pts)"
          )
          .mkString("\n")
      )

  def computeGameForEachTrumpSuit(
      players: (String, String, String, String),
      handMap: Map[String, Hand],
      precision: Int
  ) = {
    List(Spades, Diamonds, Clubs, Hearts).map(trumpSuit =>
      val optGame = Game(
        players,
        players._1,
        trumpSuit,
        handMap,
        List.empty[(Card, String)],
        List.empty[Tricks],
        List.empty[Card],
        0,
        false
      ).randomOptimizeRec(100)

      val points = optGame.computePoints
      val pointsTeamA = points(players._1)
      val pointsTeamB = points(players._2)

      val result = OptimizedGameResult(
        players,
        handMap,
        trumpSuit,
        pointsTeamA,
        pointsTeamB
      )
      result.toMap
    )
  }

  // def generate
}
