package core.game.roundTree

import core.game.Game
import core.game.cards.{Card, Hand, Suit}

import scala.annotation.tailrec

case class RandomGame(tricks: List[Tricks]) {
  def computePoints: (Int, Int) = (
    tricks
      .filter(t => (t.wonBy == "p1") || (t.wonBy == "p3"))
      .map(_.points)
      .sum,
    tricks
      .filter(t => (t.wonBy == "p2") || (t.wonBy == "p4"))
      .map(_.points)
      .sum
  )
}

object RandomGameGenerator {
  @tailrec
  def computeRec(
      game: Game,
      currentPlayer: String,
      playedCard: List[(Card, String)],
      tricksList: List[Tricks] = List.empty[Tricks]
  ): RandomGame = {
    val (card, rest) = PlayableCards(
      game,
      currentPlayer,
      game.handMap(currentPlayer).cards,
      playedCard,
      game.trumpSuit,
      playedCard.headOption.map(_._1.suit).getOrElse(Suit.None)
    ).generateRandomWithRest

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
  }

  def generateRec(
      game: Game,
      trumpSuit: Suit,
      handMap: Map[String, Hand],
      lastTrick: Option[Tricks] = None,
      tricksList: List[Tricks] = List.empty[Tricks]
  ): RandomGame = {
    val firstPlayer = lastTrick match
      case Some(trick) => trick.wonBy
      case None        => game.players._1

    val secondPlayer = game.nextPlayer(firstPlayer)
    val thirdPlayer = game.nextPlayer(secondPlayer)
    val fourthPlayer = game.nextPlayer(thirdPlayer)

    val (firstCard, firstRest) = PlayableCards(
      game,
      firstPlayer,
      handMap(firstPlayer).cards,
      List.empty[(Card, String)],
      trumpSuit,
      Suit.None
    ).generateRandomWithRest

    val (secondCard, secondRest) = PlayableCards(
      game,
      secondPlayer,
      handMap(secondPlayer).cards,
      List((firstCard, firstPlayer)),
      trumpSuit,
      firstCard.suit
    ).generateRandomWithRest

    val (thirdCard, thirdRest) = PlayableCards(
      game,
      thirdPlayer,
      handMap(thirdPlayer).cards,
      List((firstCard, firstPlayer), (secondCard, secondPlayer)),
      trumpSuit,
      firstCard.suit
    ).generateRandomWithRest

    val (fourthCard, fourthRest) = PlayableCards(
      game,
      fourthPlayer,
      handMap(fourthPlayer).cards,
      List(
        (firstCard, firstPlayer),
        (secondCard, secondPlayer),
        (thirdCard, thirdPlayer)
      ),
      trumpSuit,
      firstCard.suit
    ).generateRandomWithRest

    val playedCards = List(
      (firstCard, firstPlayer),
      (secondCard, secondPlayer),
      (thirdCard, thirdPlayer),
      (fourthCard, fourthPlayer)
    )

    firstRest match
      case Nil =>
        RandomGame(
          tricksList.appended(Tricks.fromCards(playedCards, trumpSuit, true))
        )
      case cards =>
        RandomGame(
          tricksList.appended(
            Tricks.fromCards(playedCards, trumpSuit)
          ) ++ generateRec(
            game,
            trumpSuit,
            Map(
              firstPlayer -> Hand(firstRest),
              secondPlayer -> Hand(secondRest),
              thirdPlayer -> Hand(thirdRest),
              fourthPlayer -> Hand(fourthRest)
            ),
            Some(Tricks.fromCards(playedCards, trumpSuit))
          ).tricks
        )

  }
}
