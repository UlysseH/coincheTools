package core.game.roundTree

import core.game.Game
import core.game.cards.*

case class GameTreeNode(
    game: Game,
    currentPlayer: String,
    currentSuit: Suit,
    trumpSuit: Suit,
    handMap: Map[String, Hand],
    // Current winning player at head
    cardsPlayed: List[(Card, String)],
    children: List[GameTreeNode],
    listOfPlayedCard: List[Card] = List.empty[Card],
    isLeaf: Boolean = false
) {
  val playableCards: List[Card] = PlayableCards(
    game,
    currentPlayer,
    handMap(currentPlayer).cards,
    cardsPlayed,
    trumpSuit,
    currentSuit
  ).cards

  private def takesLead(card: Card): Boolean =
    if (card.suit.==(trumpSuit))
      cardsPlayed
        .filter(_._1.suit.==(trumpSuit))
        .map(_._1.height.getTrumpRank)
        .forall(x => card.height.getTrumpRank > x)
    else {
      if (cardsPlayed.exists(_._1.suit.==(trumpSuit))) false
      else
        cardsPlayed
          .filter(_._1.suit.==(currentSuit))
          .map(_._1.height.getBaseRank)
          .forall(x => card.height.getBaseRank > x)
    }

  def generateChildren: List[GameTreeNode] = playableCards.map(card =>
    GameTreeNode(
      game,
      game.nextPlayer(currentPlayer),
      if (currentSuit.==(Suit.None)) card.suit else currentSuit,
      trumpSuit,
      handMap.map((s, h) =>
        if (s == currentPlayer) (s, h.remove(card)) else (s, h)
      ),
      if (takesLead(card)) cardsPlayed.prepended((card, currentPlayer))
      else cardsPlayed.appended((card, currentPlayer)),
      List.empty[GameTreeNode],
      listOfPlayedCard = listOfPlayedCard.appended(card)
    )
  )

  def compute: GameTreeNode = {
    // println(listOfPlayedCard.length)
    cardsPlayed.length match {
      case 4 =>
        this.copy(cardsPlayed = List.empty[(Card, String)]).compute
      case _ if handMap.toList.map(_._2.cards.length).sum == 0 =>
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
