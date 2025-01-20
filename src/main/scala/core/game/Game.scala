package core.game

import cats.effect.IO
import core.game.GameOutputs.ForOptimizeAnalysis
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
    isComplete: Boolean,
    forbiddenHandMap: Map[Player, List[Card]]
) {
  def printInfo: IO[Unit] = handMap.printInfo(trumpSuit)

  // def

  /** Playable Cards functionalities
    */
  val currentSuit: Suit =
    cardsInPlay.headOption.map(_._1.suit).getOrElse(Suit.None)

  val playerCards: List[Card] = handMap.getPlayerCards(currentPlayer)


  /** TODO: Instead of computing separately forbidden hand map and playable
   * card, each playable card case implies potential future forbidden cards
   */
  val playableCards: List[Card] = {
    val partnerIsMaster: Boolean =
      cardsInPlay
        .sortBy((card, _) =>
          if (cardsInPlay.head._1.suit == trumpSuit) card.height.getTrumpRank
          else card.height.getBaseRank
        )
        .headOption
        .exists((_, player) => player.partner == currentPlayer)
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

    /** TODO: Instead of computing separately forbidden hand map and playable
      * card, each playable card case implies potential future forbidden cards
      */

    val newForbiddenHandMap =
      forbiddenHandMap.updated(currentPlayer, (forbiddenHandMap(currentPlayer)))

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

  final def generateRandomGameFromNextCard(card: Card): Game = if (isComplete)
    this
  else
    nextStep(card).generateRandomGameFromHere

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

  def generateRandomOptGame(
      precision: Int
  ): Option[(Result, ForOptimizeAnalysis)] = {
    if (handMap.initial) {
      val (resGame, data) = randomOptimizeRec(
        precision,
        ForOptimizeAnalysis(
          (
            handMap.cards1.map(_.getNotation),
            handMap.cards2.map(_.getNotation),
            handMap.cards3.map(_.getNotation),
            handMap.cards4.map(_.getNotation)
          ),
          Nil,
          Nil
        )
      )
      Some(
        (
          Result(
            handMap,
            trumpSuit,
            resGame.computePoints(player1),
            resGame.computePoints(player2),
            resGame.cardsFromBeginning,
            resGame.tricksList
          ),
          data
        )
      )
    } else None
  }

  def randomOptimizeRec(
      precision: Int,
      data: ForOptimizeAnalysis
  ): (Game, ForOptimizeAnalysis) =
    if (isComplete) {
      (this, data)
    } else {
      val res = playableCards.flatMap(card =>
        List.fill(precision)(
          generateRandomAdjacentHandMapFromHere.generateRandomGameFromNextCard(
            card
          )
        )
      )

      val points = res
        .groupBy(_.cardsFromBeginning(step))
        .toList
        .map((card, games) => (card, games.map(_.computePoints(currentPlayer))))

      val bestCardChoice =
        points.map((card, l) => (card, l.sum)).sortBy(_._2).reverse.head._1

      val dataInc =
        data.copy(
          playedCards = data.playedCards.appended(bestCardChoice.getNotation),
          vectors = data.vectors ++ points
            .map((card, pts) =>
              (
                step,
                card.getNotation,
                pts
                /*.groupBy(x => x)
                .toList
                .map((x, l) => (x, l.length))
                .sortBy(_._1)*/
              )
            )
            .sortBy(_._3.sum)
            .reverse
        )

      nextStep(bestCardChoice).randomOptimizeRec(precision, dataInc)
    }

  /*def bucketCounts(points: List[Int]) = List(0, 82, 90, 100, 110, 120, 130, 140, 150, 160).map(
    floor => if
  )*/

  def standardDeviation(points: List[Int], mean: Int): BigDecimal = BigDecimal(
    math.sqrt(
      points
        .map(pts => math.pow(pts.toDouble - mean.toDouble, 2))
        .sum / points.length
    )
  ).setScale(2, RoundingMode.HALF_DOWN)

  /*def generateAllTricksWithPoints(
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
  }*/

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
    false,
    Map.empty[Player, List[Card]]
  )

  def everySuitFromHandMap(hMap: HandMap): List[Game] =
    List(Spades, Hearts, Diamonds, Clubs).map(suit =>
      Game(
        player1,
        suit,
        hMap,
        Nil,
        Nil,
        Nil,
        0,
        false,
        Map.empty[Player, List[Card]]
      )
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
      false,
      Map.empty[Player, List[Card]]
    )
  }
}
