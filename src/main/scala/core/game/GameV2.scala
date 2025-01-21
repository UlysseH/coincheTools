/*
package core.game

import core.game.cards.{HandMap, Suit}

case class GameV2(
    id: String,
    startingHandMap: HandMap,
    trumpSuit: Suit
) {
  def generateRandomGameFromGameState(
      gameState: GameStateReformated,
      forbiddenHandMap: HandMap
  ) = {}

  def generateRandomHandMapFromStateAndForbiddenExceptPlayer(
      gameState: GameStateReformated,
      forbiddenHandMap: HandMap,
      player: Player
  ) = {
    val handMap = gameState.handMap
    val restCard = Player.values.toList.filterNot(_.==(player)).flatMap(player => HandMap.)
  }
}
 */
