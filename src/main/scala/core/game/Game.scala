package core.game

import cats.effect.IO
import core.game.Player.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import core.game.cards.{Card, Deck, DistributePattern, Hand, HandMap, Suit}
import core.game.roundTree.Tricks
import io.circe.syntax.*

import scala.annotation.tailrec
import scala.util.Random
import scala.math
import scala.math.BigDecimal.RoundingMode

case class Game(
    currentPlayer: Player,
    trumpSuit: Suit,
    handMap: HandMap,
    cardsInPlay: List[(Card, Player)],
    tricksList: List[Tricks],
    cardsFromBeginning: List[Card],
    step: Int,
    isComplete: Boolean
) {
  def printInfo: IO[Unit] = handMap.printInfo(trumpSuit)

  // def

  /** Playable Cards functionalities
    */
  val currentSuit: Suit =
    cardsInPlay.headOption.map(_._1.suit).getOrElse(Suit.None)

  val playerCards: List[Card] = handMap.getPlayerCards(currentPlayer)

  val playableCards: List[Card] = {
    val partnerIsMaster: Boolean =
      cardsInPlay.headOption.exists((_, player) =>
        player.partner == currentPlayer
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
    val _ = handMap
    val newHandMap = handMap.removeCard(cardPlayed, currentPlayer)

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
          cardsInPlay = List.empty[(Card, Player)],
          newTricks,
          cardsFromBeginning = cardsFromBeginning.appended(cardPlayed),
          step = step + 1,
          tricksList.length == 7
        )
      }
      case _ =>
        this.copy(
          currentPlayer = currentPlayer.nextPlayer,
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
  def computePoints: Map[Player, Int] = {
    val pointsA = tricksList
      .filter(t => (t.wonBy == player1) || (t.wonBy == player3))
      .map(_.points)
      .sum
    val pointsB = tricksList
      .filter(t => (t.wonBy == player2) || (t.wonBy == player4))
      .map(_.points)
      .sum
    Map(
      player1 -> pointsA,
      player2 -> pointsB,
      player3 -> pointsA,
      player4 -> pointsB
    )
  }

  def printPoints: IO[Unit] = IO.println(
    s"\n[points] Team 1: ${computePoints(player1)} | Team 2: ${computePoints(player2)}"
  )

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
        handMap.toList.filterNot(_._1.==(currentPlayer)).flatMap(_._2)
      )
    val numCardMap = handMap.toList
      .filterNot(_._1.==(currentPlayer))
      .map((player, l) => (player, l.length))
      .scanLeft((EMPTY, 0, 0))((acc, x) => (x._1, acc._3, acc._3 + x._2))
      .tail
    val newHandMap =
      numCardMap
        .map((player, x, y) => player -> shuffledRest.slice(x, y))
        .toMap
        .+(currentPlayer -> handMap.getPlayerCards(currentPlayer))
    this.copy(handMap = HandMap.fromMap(newHandMap))
  }

  def generateRandomOptGame(precision: Int): Option[Result] = {
    if (handMap.initial) {
      val resGame = randomOptimizeRec(precision)
      Some(
        Result(
          handMap,
          trumpSuit,
          resGame.computePoints(player1),
          resGame.computePoints(player2),
          resGame.cardsFromBeginning,
          resGame.tricksList
        )
      )
    } else None
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
        .map((c, l) => {
          val mean = l.map(_.computePoints(currentPlayer)).sum / l.length
          (
            c,
            l.map(_.computePoints(currentPlayer)).sum / l.length,
            standardDeviation(l.map(_.computePoints(currentPlayer)), mean)
          )
        })
        .sortBy(_._2)
        .reverse

      println(y.mkString("\n"))
      println("\n")

      nextStep(y.head._1).randomOptimizeRec(precision)
    }

  def standardDeviation(points: List[Int], mean: Int): BigDecimal = BigDecimal(
    math.sqrt(
      points
        .map(pts => math.pow(pts.toDouble - mean.toDouble, 2))
        .sum / points.length
    )
  ).setScale(2, RoundingMode.HALF_DOWN)

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
                (c1, player1),
                (c2, player2),
                (c3, player3),
                (c4, player4)
              ),
              trumpSuit
            )
            val t2 = Tricks.fromCards(
              List(
                (c2, player2),
                (c3, player3),
                (c4, player4),
                (c1, player1)
              ),
              trumpSuit
            )
            val t3 = Tricks.fromCards(
              List(
                (c3, player3),
                (c4, player4),
                (c1, player1),
                (c2, player2)
              ),
              trumpSuit
            )
            val t4 = Tricks.fromCards(
              List(
                (c4, player4),
                (c1, player1),
                (c2, player2),
                (c3, player3)
              ),
              trumpSuit
            )

            List(
              Tricks(t1.wonBy, t1.points, List(c1, c2, c3, c4), player1),
              Tricks(t2.wonBy, t2.points, List(c2, c3, c4, c1), player2),
              Tricks(t3.wonBy, t3.points, List(c3, c4, c1, c2), player3),
              Tricks(t4.wonBy, t4.points, List(c4, c1, c2, c3), player4)
            )
          )
        )
      )
    )
  }

  // val isPartner(a, b)
}

object Game {
  def fromHandMap(hMap: HandMap, trumpSuit: Suit): Game = Game(
    player1,
    trumpSuit,
    hMap,
    Nil,
    Nil,
    Nil,
    0,
    false
  )

  def init(
      trumpSuit: Suit,
      deck: Deck,
      pattern: DistributePattern
  ): Game = {
    val cutDeck = deck.cut
    val draw = cutDeck.distribute(pattern)
    val handMap = HandMap.fromDraw(draw)
    Game(
      player1,
      trumpSuit,
      handMap,
      List.empty[(Card, Player)],
      List.empty[Tricks],
      List.empty[Card],
      0,
      false
    )
  }
}
