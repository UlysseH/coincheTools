package core.game.cards

import cats.effect.IO
import core.game.{Game, Player, Result}
import core.game.Player.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}

import scala.annotation.tailrec
import scala.util.Random

case class HandMap(
    cards1: List[Card],
    cards2: List[Card],
    cards3: List[Card],
    cards4: List[Card]
) {
  def initial: Boolean =
    (cards1.length + cards2.length + cards3.length + cards4.length) == 32
  def toList: List[(Player, List[Card])] = List(
    (player1, cards1),
    (player2, cards2),
    (player3, cards3),
    (player4, cards4)
  )
  def toMap: Map[Player, List[Card]] = toList.map((p, c) => p -> c).toMap
  def getPlayerCards(player: Player): List[Card] = player match
    case Player.player1 => cards1
    case Player.player2 => cards2
    case Player.player3 => cards3
    case Player.player4 => cards4

  def toStringOrderedByTrumpSuit(trumpSuit: Suit): String = toList
    .map((s, l) =>
      (
        s,
        Hand(
          l
            .sortBy(card =>
              if (card.suit == trumpSuit) card.height.getTrumpRank + 100
              else card.height.getBaseRank
            )
            .reverse
        )
      )
    )
    .map((p, h) =>
      s"$p : ${h.cards.map(_.toString).mkString(",")} (${h
          .countPoints(Suit.Spades)}pts)"
    )
    .prepended(s"[info] p1 took with $trumpSuit")
    .mkString("\n")

  def removeCard(card: Card, player: Player): HandMap = player match
    case Player.player1 => this.copy(cards1 = cards1.filterNot(_.==(card)))
    case Player.player2 => this.copy(cards2 = cards2.filterNot(_.==(card)))
    case Player.player3 => this.copy(cards3 = cards3.filterNot(_.==(card)))
    case Player.player4 => this.copy(cards4 = cards4.filterNot(_.==(card)))

  def toStringMap: Map[String, String] = Map(
    "cards1" -> cards1.map(_.toString).mkString(","),
    "cards2" -> cards2.map(_.toString).mkString(","),
    "cards3" -> cards3.map(_.toString).mkString(","),
    "cards4" -> cards4.map(_.toString).mkString(",")
  )

  def printInfo(trumpSuit: Suit): IO[Unit] =
    IO.println(s"${if (initial) "[Initial]" else "[Playing]"} players/cards") *>
      IO.println(
        toList
          .map((s, l) =>
            (
              s,
              Hand(
                l
                  .sortBy(card =>
                    if (card.suit == trumpSuit) card.height.getTrumpRank + 100
                    else card.height.getBaseRank
                  )
                  .reverse
              )
            )
          )
          .map((p, h) =>
            s"$p : ${h.cards.map(_.toString).mkString(",")} (${h
                .countPoints(Suit.Spades)}pts)"
          )
          .mkString("\n")
      )

  def randomExceptPlayer(
      currentPlayer: Player,
      forbiddenMap: Map[Player, List[Card]]
  ): HandMap = {
    val handMap = toMap
    val shuffledRest =
      Random.shuffle(
        handMap.filterNot(_._1.==(currentPlayer)).flatMap(_._2)
      )
    val numCardMap = handMap.toList
      .filterNot(_._1.==(currentPlayer))
      .map((player, l) => (player, l.length))
      .scanLeft((EMPTY, 0, 0))((acc, x) => (x._1, acc._3, acc._3 + x._2))
      .tail

    val listOfDecksToChooseFrom =
      numCardMap.scanLeft(shuffledRest)((deck, elt) =>
        deck.filterNot(card =>
          forbiddenMap(elt._1).contains(card)
        ) ++ forbiddenMap(elt._1)
      )

    val x =
      numCardMap.scanLeft((shuffledRest, List.empty[Card]))((acc, elt) => {
        val player = elt._1
        val rest = acc._1
        val restForbiddenCardsRearranged = (rest.filterNot(card =>
          forbiddenMap(player).contains(card)
        ) ++ forbiddenMap(player)).toList
        (
          restForbiddenCardsRearranged
            .slice(8, restForbiddenCardsRearranged.length),
          restForbiddenCardsRearranged.slice(0, 8)
        )
      })

    val newHandMap =
      numCardMap
        .map((player, x, y) => {
          player -> shuffledRest.slice(x, y)
        })
        .toMap
        .+(currentPlayer -> handMap(currentPlayer))
    HandMap(
      newHandMap(player1).toList,
      newHandMap(player2).toList,
      newHandMap(player3).toList,
      newHandMap(player4).toList
    )
  }

  def getBestExpectedResultTeamA(precision: Int): Result = {
    val games = List(Spades, Diamonds, Clubs, Hearts).map(suit =>
      Game.fromHandMap(this, suit)
    )
    val bestGame =
      games
        .flatMap(_.generateRandomOptGame(precision))
        .map(_._1)
        .maxBy(_.pointsA)
    bestGame
  }

  def optimizeValuesForPlayerHand(
      player: Player,
      numSamples: Int,
      precision: Int,
      forbiddenHandMap: Map[Player, List[Card]]
  ): List[Result] = {
    val samples =
      List.fill(numSamples)(randomExceptPlayer(player, forbiddenHandMap))
    val games = samples.flatMap(hMap =>
      List(Spades, Diamonds, Clubs, Hearts).map(suit =>
        Game.fromHandMap(hMap, suit)
      )
    )
    val results = games.flatMap(_.generateRandomOptGame(precision)).map(_._1)

    results
  }

  def genNewHandMapWithForbidden(
      player: Player,
      fHandMap: Map[Player, List[Card]],
      step: Int = 0
  ): Option[HandMap] = {
    val handSizeMap: Map[Player, Int] =
      toMap.filterNot(_._1.==(player)).map((p, l) => (p, l.length))

    val rest =
      Random.shuffle(toMap.filterNot(_._1.==(player)).toList.flatMap(_._2))
    // println(rest.map(_.toString).mkString(","))

    takeOneAndPassToNextV2(
      player.nextPlayer,
      handSizeMap.map((p, _) => (p, List.empty[Card])),
      Random.shuffle(rest)
    )(fHandMap, handSizeMap) match
      case Some(value) => {
        val resMap = HandMap(
          value._1.getOrElse(player1, cards1),
          value._1.getOrElse(player2, cards2),
          value._1.getOrElse(player3, cards3),
          value._1.getOrElse(player4, cards4)
        )
        // println(s"[info] drafted in $numSteps steps")
        Some(resMap)
      }
      case None => {
        if (step <= 100) genNewHandMapWithForbidden(player, fHandMap, step + 1)
        else {
          println(
            "[failed] to draft in less than 100 step -> retry from beginning"
          )
          None
        }
      }
  }

  /** The idea is for each player to pick an allowed card in the rest pile, and
    * pass it to the next player Because we exchange when hand is full and
    * randomize in between, this should not block as in previous version
    */
  def takeOneAndPassToNextV2(
      player: Player,
      // hMap is the card map of three Players (used for optimizing a player game)
      hMap: Map[Player, List[Card]],
      rest: List[Card],
      step: Int = 0
  )(implicit
      fHandMap: Map[Player, List[Card]],
      handSizeMap: Map[Player, Int]
  ): Option[(Map[Player, List[Card]], Int)] = {

    /** If all player have the right number of cards in their respective hands,
      * return the result
      */
    if (hMap.toList.forall((p, l) => l.length == handSizeMap(p)))
      Some((hMap, step))
    else {
      if (step >= 100) None
      else {
        val forbidden = fHandMap(player)
        val optCard = rest.collectFirst({
          case card if !forbidden.contains(card) => card
        })

        /** since one player has a fixed hand (current player whose turn it is),
          * we might skip one next player
          */
        val nextPlayer =
          if (handSizeMap.isDefinedAt(player.nextPlayer)) player.nextPlayer
          else player.nextPlayer.nextPlayer

        (hMap(player).length == handSizeMap(player), optCard) match {
          /** Most common case : player hand is not full yet and at least one
            * card is matching allowed cards
            */
          case (false, Some(card)) =>
            takeOneAndPassToNextV2(
              nextPlayer,
              hMap.updated(player, hMap(player).appended(card)),
              rest.filterNot(_.==(card)),
              step + 1
            )

          /** player hand is full but there is at least one card in the rest
            * that matches -> exchange it with one card in hand
            */
          case (true, Some(card)) =>
            Random.shuffle(hMap(player)).headOption match
              case Some(value) =>
                takeOneAndPassToNextV2(
                  nextPlayer,
                  hMap.updated(
                    player,
                    hMap(player).filterNot(_.==(value)).appended(card)
                  ),
                  rest.filterNot(_.==(card)).appended(value),
                  step + 1
                )

              /** At the end of the game, expected hand size might be 0, in this
                * case we must skip to next player
                */
              case None =>
                takeOneAndPassToNextV2(nextPlayer, hMap, rest, step + 1)

          /** If no card matches for player, move to next (since we exchange
            * cards, next turn player might find a suitable card)
            */
          case (_, None) =>
            takeOneAndPassToNextV2(
              nextPlayer,
              hMap,
              rest,
              step + 1
            )
        }
      }

    }
  }

  /*/** The idea is for each player to pick an allowed card in the rest pile, and
   * pass it to the next player If player cannot pick a card from the rest,
   * s.he will take it from an other player hand Notes:
   *   - randomizing seems to double the time taken by the function -> might
   *     not be necessary
   */
  def takeOneAndPassToNext(
      player: Player,
      // hMap is the card map of three Players (used for optimizing a player game)
      hMap: Map[Player, List[Card]],
      rest: List[Card]
  )(implicit
      fHandMap: Map[Player, List[Card]],
      handSizeMap: Map[Player, Int]
  ): Map[Player, List[Card]] = {
    // println(handSizeMap)
    // println(hMap.map((p, l) => (p, l.map(_.toString).mkString(","))))
    // println(fHandMap.map((p, l) => (p, l.map(_.toString).mkString(","))))
    // println(rest.map(_.toString).mkString(","))
    // println(hMap.map((p, l) => (p, l.map(_.toString).mkString(","))))

    /** If all player have the right number of cards in their respective hands,
   * return the result
   */
    if (hMap.toList.forall((p, l) => l.length == handSizeMap(p))) hMap
    else {

      /** Even if the player has enough cards in his hand, if s.he can take a
   * new one, exchange it with a card in his/her hand to avoid blocking
   */
      if (hMap(player).length == handSizeMap(player))
        println(player)

        takeOneAndPassToNext(
          /** since one player has a fixed hand (current player whose turn it
   * is), we might skip one next player
   */
          if (handSizeMap.isDefinedAt(player.nextPlayer)) player.nextPlayer
          else player.nextPlayer.nextPlayer,
          hMap,
          // Random.shuffle(rest)
          rest
        )
      else {
        val forbidden = fHandMap(player)
        val nextStep = rest.collectFirst({
          case card if !forbidden.contains(card) =>
            takeOneAndPassToNext(
              if (handSizeMap.isDefinedAt(player.nextPlayer)) player.nextPlayer
              else player.nextPlayer.nextPlayer,
              hMap.updated(
                player,
                hMap(player).appended(card)
              ),
              // Random.shuffle(rest.filterNot(_.==(card)))
              rest.filterNot(_.==(card))
            )
        })
        nextStep match
          case Some(value) =>
            value
          case None =>
            println(s"$player: ${rest.map(_.toString).mkString(",")}")
            takeOneAndPassToNext(
              if (handSizeMap.isDefinedAt(player.nextPlayer)) player.nextPlayer
              else player.nextPlayer.nextPlayer,
              takeInPlayerHand(
                player,
                forbidden,
                hMap,
                player.nextPlayer
              ),
              // Random.shuffle(rest)
              rest
            )
      }
    }
  }

  /** If a player needs a card but cannot find a suitable one in the rest, s.he
   * will look for it in another player hand
   */
  @tailrec
  final def takeInPlayerHand(
      stealingPlayer: Player,
      forbiddenCards: List[Card],
      hMap: Map[Player, List[Card]],
      targetedPlayer: Player
  ): Map[Player, List[Card]] =
    if (hMap.isDefinedAt(targetedPlayer)) {
      Random
        .shuffle(hMap(targetedPlayer))
        .collectFirst({
          case card if !forbiddenCards.contains(card) =>
            println(
              s"$stealingPlayer stole ${card.toString} to $targetedPlayer ($targetedPlayer has ${hMap(
                  targetedPlayer
                ).map(_.toString).mkString(",")})"
            )
            hMap
              .updated(
                stealingPlayer,
                hMap(stealingPlayer).appended(card)
              )
              .updated(
                targetedPlayer,
                hMap(targetedPlayer).filterNot(_.==(card))
              )
        }) match
        case Some(value) => value
        case None =>
          takeInPlayerHand(
            stealingPlayer,
            forbiddenCards,
            hMap,
            if (targetedPlayer.nextPlayer == stealingPlayer)
              stealingPlayer.nextPlayer
            else targetedPlayer.nextPlayer
          )
    } else
      /** This is necessary if the targeted player is the fixed
   * (current/playing) player
   */
      takeInPlayerHand(
        stealingPlayer,
        forbiddenCards,
        hMap,
        targetedPlayer.nextPlayer
      )*/
}

object HandMap {
  // TODO: this is obviously not safe, find corrections
  def fromMap(m: Map[Player, List[Card]]): HandMap =
    HandMap(m(player1), m(player2), m(player3), m(player4))
  def fromDraw(draw: Draw): HandMap =
    HandMap(draw.h1.cards, draw.h2.cards, draw.h3.cards, draw.h4.cards)
  def fromStrings(s1: String, s2: String, s3: String, s4: String): HandMap =
    HandMap(
      s1.split(",").toList.map(s => Card.fromLitteral(s)),
      s2.split(",").toList.map(s => Card.fromLitteral(s)),
      s3.split(",").toList.map(s => Card.fromLitteral(s)),
      s4.split(",").toList.map(s => Card.fromLitteral(s))
    )

  def random: HandMap = {
    val shuffledDeck = Deck.shuffled
    HandMap(
      shuffledDeck.cards.slice(0, 8),
      shuffledDeck.cards.slice(8, 16),
      shuffledDeck.cards.slice(16, 24),
      shuffledDeck.cards.slice(24, 32)
    )
  }
}
