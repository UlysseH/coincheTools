package core.game

import core.game.Player.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import core.game.cards.{Card, Hand, HandMap, Suit}
import core.game.roundTree.Tricks

case class GameV2(
    id: String,
    startingHandMap: HandMap,
    trumpSuit: Suit
) {
  def initialGameState: GameState =
    GameState(
      id,
      1,
      List.empty[(Card, Player)],
      player1,
      startingHandMap,
      Suit.None,
      trumpSuit,
      None,
      None
    )

  // This is the function used to randomly generate a game from a Game State
  def generateRandomGameWithFixedHandMap(
      gameState: GameState,
      forbiddenHandMap: Map[Player, List[Card]]
  ): GameState = {
    // println(gameState.step)
    // println
    val handMap = gameState.remainingHandMap
    if (handMap.toMap.toList.map(_._2.length).sum == 0) gameState
    else {
      // println(handMap.toMap.map((p, l) => (p, l.length)))
      // println(gameState.currentPlayer)
      // Compute one playable card for the player, new forbidden cards are not relevant here since the HandMap is fixed
      val (randomPlayableCard, _, takesLead) =
        gameState.generateRandomPlayableCardWithCorrelated

      val nextGameState = gameState.computeNextGameState(
        randomPlayableCard,
        takesLead
      )

      // Iterating to next player
      generateRandomGameWithFixedHandMap(
        nextGameState,
        forbiddenHandMap
      )
    }

  }

  def optimizeRecFromGameState(
      // this function is used only for optimize rec, and therefore one player hand must be fixed
      gameState: GameState,
      forbiddenHandMap: Map[Player, List[Card]],
      precision: Int
  ): Option[GameState] = {
    // println(gameState.step)
    val handMap = gameState.remainingHandMap
    if (handMap.toMap.toList.map(_._2.length).sum == 0) Some(gameState)
    else {
      val currentPlayer = gameState.currentPlayer

      /** For each playable hand, we generate a number of random games (with
        * different starting HandMap)
        */
      val playableList = gameState.generatePlayableCards

      val resultsOfSimulationsForEachPlayableCard = {
        playableList.map((playableCard, forbiddenCards, takesLead) => {

          /** First, we compute the gamestate after the card is played, and the
            * forbiddenHandMap related for future generations
            */
          val nextState =
            gameState.computeNextGameState(playableCard, takesLead)

          val newForbiddenHandMap = forbiddenHandMap.updated(
            currentPlayer,
            forbiddenHandMap(currentPlayer)
              .++(forbiddenCards)
              .distinct
          )

          /** We then generate a bunch of random games from the position, to
            * chose the best card to play. For each, we generate a random
            * adjacent HandMap to avoid biased decision from one setup
            */
          val randomGamesResults = List.fill(precision)({

            /** Generate a new hand map were opponents hands are reshuffled,
              * taking previous blocking actions into account
              */
            handMap
              .genNewHandMapWithForbidden(
                currentPlayer,
                newForbiddenHandMap
              ) match
              case Some(newHandMap) => {

                /** Then we input the Randomly generated HandMap to the
                  * GameState
                  */
                val randomizedGameState =
                  gameState
                    .copy(
                      remainingHandMap =
                        newHandMap.removeCard(playableCard, currentPlayer)
                    )
                    .computeNextGameState(playableCard, takesLead)

                /** Computing the tricks and the points associated
                  */
                val tricks = GameV2
                  .computeTricksFromGameState(
                    generateRandomGameWithFixedHandMap(
                      randomizedGameState,
                      forbiddenHandMap
                    )
                  )
                  .get
                val points =
                  Tricks.computePoints(tricks, gameState.currentPlayer)

                Some(points)
              }
              case None => None

          })
          if (randomGamesResults.exists(_.isEmpty)) None
          else
            Some(
              (
                playableCard,
                nextState,
                newForbiddenHandMap,
                randomGamesResults.map(_.get).sum / precision
              )
            )
        })
      }

      /*println(
        gameState.playedCardsWithPlayers
          .map((card, player) => s"${card.getNotation}($player)")
          .mkString(",")
      )
      println(
        resultsOfSimulationsForEachPlayableCard
          .map(elt => (elt._1.getNotation, elt._4))
          .sortBy(_._2)
          .reverse
      )
      println("\n")*/

      if (resultsOfSimulationsForEachPlayableCard.exists(_.isEmpty)) None
      else {
        val (playedCard, chosenGameState, newForbiddenHandMap) =
          (
            resultsOfSimulationsForEachPlayableCard.map(_.get).maxBy(_._4)._1,
            resultsOfSimulationsForEachPlayableCard.map(_.get).maxBy(_._4)._2,
            resultsOfSimulationsForEachPlayableCard.map(_.get).maxBy(_._4)._3
          )

        /** We then iterate to next player to play
          */
        optimizeRecFromGameState(
          chosenGameState,
          newForbiddenHandMap,
          precision
        )
      }
    }

  }
}

object GameV2 {
  val initialForbiddenHandMap = Map(
    player1 -> List.empty[Card],
    player2 -> List.empty[Card],
    player3 -> List.empty[Card],
    player4 -> List.empty[Card]
  )

  def computeTricksFromGameState(
      gameState: GameState
  ): Option[List[Tricks]] = {
    if (gameState.isFinal) {
      val playedCardsWithPlayers =
        gameState.playedCardsWithPlayers
      val grouped = playedCardsWithPlayers.grouped(4).toList
      if (grouped.length == 8 && grouped.map(_.length).sum == 32)
        Some(
          grouped.reverse.tail.reverse
            .map(l => Tricks.fromCards(l, gameState.trumpSuit))
            .appended(Tricks.fromCards(grouped.last, gameState.trumpSuit, true))
        )
      else {
        println("ERROR with Random Game Generation : the Game is inconsistent")
        None
      }
    } else {
      println("ERROR with Random Game Generation : the GameState is not FINAL")
      None
    }

  }
  case class GameResultStr(
      id: String,
      cards1: String,
      cards2: String,
      cards3: String,
      cards4: String,
      cardsPlayed: String,
      trumpSuit: String,
      pointsA: Int,
      pointsB: Int,
      precision: Int,
      algoVersion: Int
  )

  def randomHandMapAllSuitsToGameResultStr(
      precision: Int
  ): List[GameResultStr] = {
    val games = randomHandMapAllSuits(precision)
    games.map((hMap, gameState) => {
      val tricks = computeTricksFromGameState(gameState)
        .map(tricks => (tricks, gameState.trumpSuit))
        .get
        ._1
      val pointsA = Tricks.computePoints(tricks, player1)
      val pointsB = Tricks.computePoints(tricks, player2)
      GameResultStr(
        gameState.gameId,
        Hand(hMap.cards1)
          .toStringTrumpOrdered(gameState.trumpSuit)
          .mkString(","),
        Hand(hMap.cards2)
          .toStringTrumpOrdered(gameState.trumpSuit)
          .mkString(","),
        Hand(hMap.cards3)
          .toStringTrumpOrdered(gameState.trumpSuit)
          .mkString(","),
        Hand(hMap.cards4)
          .toStringTrumpOrdered(gameState.trumpSuit)
          .mkString(","),
        gameState.playedCardsWithPlayers.map(_._1.getNotation).mkString(","),
        gameState.trumpSuit.toString,
        pointsA,
        pointsB,
        precision,
        algoVersion = 0
      )
    })
  }

  def randomHandMapAllSuits(precision: Int): List[(HandMap, GameState)] = {
    val uuid = java.util.UUID.randomUUID.toString

    val handMap = HandMap.random
    val games = List(
      GameV2(s"$uuid-spades", handMap, Spades),
      GameV2(s"$uuid-hearts", handMap, Hearts),
      GameV2(s"$uuid-clubs", handMap, Clubs),
      GameV2(s"$uuid-diamonds", handMap, Diamonds)
    )
    games
      .map(game =>
        game.optimizeRecFromGameState(
          game.initialGameState,
          initialForbiddenHandMap,
          precision
        ) match
          case Some(value) => Some((handMap, value))
          case None        => None
      )
      .map(x => x)
      .collect { case Some(value) =>
        value
      }
  }

  def randomGamesAllSuitsRandomHandMapIsolateBest(
      player: Player,
      precision: Int
  ): (Int, Suit) = {
    def uuid = java.util.UUID.randomUUID.toString
    val handMap = HandMap.random
    val games = List(
      GameV2(s"$uuid-spades", handMap, Spades),
      GameV2(s"$uuid-hearts", handMap, Hearts),
      GameV2(s"$uuid-clubs", handMap, Clubs),
      GameV2(s"$uuid-diamonds", handMap, Diamonds)
    )
    val optGames = games
      .map(game =>
        game.optimizeRecFromGameState(
          game.initialGameState,
          initialForbiddenHandMap,
          precision
        )
      )
      .collect { case Some(value) =>
        value
      }
    val scoresOrderedDesc = optGames
      .flatMap(game =>
        computeTricksFromGameState(game).map(tricks => (tricks, game.trumpSuit))
      )
      .map((tricks, suit) =>
        (tricks, Tricks.computePoints(tricks, player), suit)
      )
      .sortBy(_._2)
      .reverse

    println(handMap.getNotationOrderedByTrumpSuit(scoresOrderedDesc.head._3))

    println(scoresOrderedDesc.head._1.map(_.print).mkString("\n"))

    println(
      s"[result] team1: ${scoresOrderedDesc.head._2} | team2: ${162 - scoresOrderedDesc.head._2}"
    )

    (scoresOrderedDesc.head._2, scoresOrderedDesc.head._3)
  }
}
