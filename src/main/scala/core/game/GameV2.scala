package core.game

import core.game.Player.player1
import core.game.cards.{Card, HandMap, Suit}
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
  ): GameState = {
    // println(gameState.step)
    val handMap = gameState.remainingHandMap
    if (handMap.toMap.toList.map(_._2.length).sum == 0) gameState
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
            val newHandMap =
              handMap
                .genNewHandMapWithForbidden(
                  currentPlayer,
                  newForbiddenHandMap
                )

            /** Then we input the Randomly generated HandMap to the GameState
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

            points
          })
          (
            playableCard,
            nextState,
            newForbiddenHandMap,
            randomGamesResults.sum / precision
          )
        })
      }

      println(
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
      println("\n")

      val (playedCard, chosenGameState, newForbiddenHandMap) =
        (
          resultsOfSimulationsForEachPlayableCard.maxBy(_._4)._1,
          resultsOfSimulationsForEachPlayableCard.maxBy(_._4)._2,
          resultsOfSimulationsForEachPlayableCard.maxBy(_._4)._3
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

object GameV2 {
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
}
