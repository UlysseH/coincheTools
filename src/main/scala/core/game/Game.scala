package core.game

import core.game.cards.{Card, Deck, DistributePattern, Hand, Suit}
import core.game.roundTree.Tricks

import scala.annotation.tailrec
import scala.util.Random

case class Game(
    players: (String, String, String, String),
    currentPlayer: String,
    trumpSuit: Suit,
    handMap: Map[String, Hand],
    cardsInPlay: List[(Card, String)],
    tricksList: List[Tricks],
    // step: Int,
    isComplete: Boolean
) {

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
          currentPlayer = nextPlayer(currentPlayer),
          handMap = newHandMap,
          cardsInPlay = List.empty[(Card, String)],
          newTricks,
          tricksList.length == 7
        )
      }
      case _ =>
        this.copy(
          currentPlayer = nextPlayer(currentPlayer),
          handMap = newHandMap
        )
  }

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

  /*def generateRandomGameFromHere = {
    val (card, rest) = generateRandomPlayableCardWithRest

    val newHandMap = game.handMap.updated(currentPlayer, Hand(rest))
    val newGame = game.copy(handMap = newHandMap)

    rest match
      case Nil if playedCard.length == 3 =>
        RandomGame(
          tricksList.appended(
            Tricks.fromCards(
              playedCard.appended((card, currentPlayer)),
              game.trumpSuit,
              true
            )
          )
        )
      case Nil =>
        computeRec(
          newGame,
          newGame.nextPlayer(currentPlayer),
          playedCard.appended((card, currentPlayer)),
          tricksList
        )
      case cards if playedCard.length == 3 =>
        val lastTrick = Tricks.fromCards(
          playedCard.appended((card, currentPlayer)),
          game.trumpSuit
        )
        computeRec(
          newGame,
          lastTrick.wonBy,
          List.empty[(Card, String)],
          tricksList.appended(
            Tricks.fromCards(
              playedCard.appended((card, currentPlayer)),
              game.trumpSuit
            )
          )
        )
      case cards =>
        computeRec(
          newGame,
          newGame.nextPlayer(currentPlayer),
          playedCard.appended((card, currentPlayer)),
          tricksList
        )
  }*/

  /*def randomOptimizeRec(precision: Int) = {
    val res = List.fill(precision)(
      RandomGameGenerator.computeRec(
        this,
        players._1,
        List.empty[(Card, String)]
      )
    )
  }*/

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
      false
    )
  }
}
