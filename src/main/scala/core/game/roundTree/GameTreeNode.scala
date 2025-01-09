package core.game.roundTree

import core.game.{Game, Player}
import core.game.cards.*

case class GameTreeNode(
    game: Game,
    // Current winning player at head
    children: List[GameTreeNode],
    listOfPlayedCard: List[Card] = List.empty[Card],
    isLeaf: Boolean = false
) {

  val playableCards: List[Card] = game.playableCards

  private def takesLead(card: Card): Boolean =
    if (card.suit.==(game.trumpSuit))
      game.cardsInPlay
        .filter(_._1.suit.==(game.trumpSuit))
        .map(_._1.height.getTrumpRank)
        .forall(x => card.height.getTrumpRank > x)
    else {
      if (game.cardsInPlay.exists(_._1.suit.==(game.trumpSuit))) false
      else
        game.cardsInPlay
          .filter(_._1.suit.==(game.currentSuit))
          .map(_._1.height.getBaseRank)
          .forall(x => card.height.getBaseRank > x)
    }

  def generateChildren: List[GameTreeNode] = playableCards.map(card =>
    GameTreeNode(
      game.nextStep(card),
      List.empty[GameTreeNode],
      listOfPlayedCard = listOfPlayedCard.appended(card)
    )
  )

  def compute: GameTreeNode = {
    // println(listOfPlayedCard.length)
    game.cardsInPlay.length match {
      case 4 =>
        this.copy(game.copy(cardsInPlay = List.empty[(Card, Player)])).compute
      case _ if game.handMap.toList.map(_._2.length).sum == 0 =>
        this.copy(isLeaf = true)
      case _ => this.copy(children = generateChildren.map(_.compute))
    }
  }

  def countLeaf: Long = if (this.isLeaf) 1 else children.map(_.countLeaf).sum

  def getLeaves: List[List[Card]] =
    if (this.isLeaf) List(listOfPlayedCard)
    else
      children
        .flatMap(_.getLeaves)

}
