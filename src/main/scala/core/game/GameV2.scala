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
    GameState(id, 1, player1, startingHandMap, Suit.None, trumpSuit, None, None)

  // This is the function used to randomly generate a game from a Game State
  def generateRandomGameWithFixedHandMap(
      gameState: GameState,
      forbiddenHandMap: Map[Player, List[Card]]
  ): List[(Card, Player, GameState)] = {
    // println(gameState.step)
    // println
    val handMap = gameState.remainingHandMap
    if (handMap.toMap.toList.map(_._2.length).sum == 0) Nil
    else {
      // Compute one playable card for the player, new forbidden cards are not relevant here since the HandMap is fixed
      val (randomPlayableCard, _, takesLead) =
        gameState.generateRandomPlayableCardWithCorrelated

      // Compute values for next Game State
      val nextPlayer = gameState.currentPlayer.nextPlayer
      val remainingHandMap = gameState.remainingHandMap.removeCard(
        randomPlayableCard,
        gameState.currentPlayer
      )

      val nextGameState = {
        // For when the trick is over since 4 cards have been played
        if (gameState.step % 4 == 0)
          gameState.copy(
            step = gameState.step + 1,
            currentPlayer =
              if (takesLead) gameState.currentPlayer
              else gameState.masterPlayer.get,
            remainingHandMap = remainingHandMap,
            askedSuit = Suit.None,
            masterPlayer = Some(gameState.currentPlayer),
            masterCard = Some(randomPlayableCard)
          )
        else
          // Trick is not complete
          gameState.copy(
            step = gameState.step + 1,
            currentPlayer = nextPlayer,
            remainingHandMap = remainingHandMap,
            askedSuit =
              if (gameState.askedSuit == Suit.None)
                randomPlayableCard.suit
              else gameState.askedSuit,
            masterPlayer =
              if (takesLead) Some(gameState.currentPlayer)
              else gameState.masterPlayer,
            masterCard =
              if (takesLead) Some(randomPlayableCard)
              else gameState.masterCard
          )
      }

      // Iterating to next player
      generateRandomGameWithFixedHandMap(
        nextGameState,
        forbiddenHandMap
      ).prepended((randomPlayableCard, gameState.currentPlayer, gameState))
    }

  }

  def optimizeRecFromGameState(
      // this function is used only for optimize rec, and therefore one player hand must be fixed
      gameState: GameState,
      forbiddenHandMap: Map[Player, List[Card]],
      precision: Int
  ): List[GameState] = {
    val handMap = gameState.remainingHandMap
    if (handMap.toMap.toList.map(_._2.length).sum == 0) Nil
    else {

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
            gameState.currentPlayer,
            forbiddenHandMap(gameState.currentPlayer)
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
              handMap.genNewHandMapWithForbidden(
                nextState.currentPlayer,
                newForbiddenHandMap
              )

            /** Then we input the Randomly generated HandMap to the GameState
              */
            val randomizedGameState =
              gameState.copy(remainingHandMap = newHandMap)

            /** Computing the tricks and the points associated
              */
            val tricks = GameV2
              .computeTricksFromGameStates(
                generateRandomGameWithFixedHandMap(
                  randomizedGameState,
                  forbiddenHandMap
                )
              )
              .get
            val points =
              Tricks.computePoints(tricks, randomizedGameState.currentPlayer)

            points
          })
          (randomizedGameState, randomGamesResults.sum / precision)
        })
      }

      val chosenGameState =
        resultsOfSimulationsForEachPlayableCard.maxBy(_._2)._1

      // Generate a new hand map were opponents hands are reshuffled, taking previous blocking actions into account
      val newHandMap =
        handMap.genNewHandMapWithForbidden(
          gameState.currentPlayer,
          forbiddenHandMap
        )

      // Create a new GameState with the new hand
      val randomizedGameState = gameState.copy(remainingHandMap = newHandMap)

      // Compute one playable card for the player, with future forbidden cards from playing this card and takesLead bool
      val (randomPlayableCard, newForbiddenCards, takesLead) =
        randomizedGameState.generateRandomPlayableCardWithCorrelated

      // Compute values for next Game State
      val nextPlayer = randomizedGameState.currentPlayer.nextPlayer
      val remainingHandMap = randomizedGameState.remainingHandMap.removeCard(
        randomPlayableCard,
        randomizedGameState.currentPlayer
      )
      val newForbiddenHandMap = forbiddenHandMap.updated(
        randomizedGameState.currentPlayer,
        (forbiddenHandMap(
          randomizedGameState.currentPlayer
        ) ++ newForbiddenCards).distinct
      )

      println(newHandMap.toStringMap)
      println(
        s"${gameState.currentPlayer} -> ${randomPlayableCard.getNotation} (${gameState.askedSuit} asked)"
      )
      println(
        newForbiddenHandMap.map((p, l) =>
          (p, l.map(_.getNotation).mkString(","))
        )
      )

      val nextGameState =
        randomizedGameState.computeNextGameState(randomPlayableCard, takesLead)

      /*val nextGameState = {
        // For when the trick is over since 4 cards have been played
        if (randomizedGameState.step % 4 == 0)
          randomizedGameState.copy(
            step = randomizedGameState.step + 1,
            currentPlayer =
              if (takesLead) randomizedGameState.currentPlayer
              else randomizedGameState.masterPlayer.get,
            remainingHandMap = remainingHandMap,
            askedSuit = Suit.None,
            masterPlayer = Some(randomizedGameState.currentPlayer),
            masterCard = Some(randomPlayableCard)
          )
        else
          // Trick is not complete
          randomizedGameState.copy(
            step = randomizedGameState.step + 1,
            currentPlayer = nextPlayer,
            remainingHandMap = remainingHandMap,
            askedSuit =
              if (randomizedGameState.askedSuit == Suit.None)
                randomPlayableCard.suit
              else randomizedGameState.askedSuit,
            masterPlayer =
              if (takesLead) Some(randomizedGameState.currentPlayer)
              else randomizedGameState.masterPlayer,
            masterCard =
              if (takesLead) Some(randomPlayableCard)
              else randomizedGameState.masterCard
          )
      }*/

      // Iterating to next player
      optimizeRecFromGameState(
        nextGameState,
        newForbiddenHandMap,
        precision
      )
    }

  }
}

object GameV2 {
  def computeTricksFromGameStates(
      gameStates: List[(Card, Player, GameState)]
  ): Option[List[Tricks]] = {
    val playedCardsWithPlayers =
      gameStates.map((card, player, _) => (card, player))
    val grouped = playedCardsWithPlayers.grouped(4).toList
    if (grouped.length == 8 && grouped.map(_.length).sum == 32)
      Some(
        grouped
          .map(l => Tricks.fromCards(l, gameStates.head._3.trumpSuit))
      )
    else {
      println("ERROR with Random Game Generation : incomplete or uneven game")
      None
    }

  }
}
