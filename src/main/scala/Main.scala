import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto.*
import io.circe.syntax.*
import core.game.{Game, GameV2, Player, roundTree}
import core.game.cards.{Card, Deck, DistributePattern, Hand, HandMap, Suit}
import core.game.cards.DistributePattern.*
import core.game.cards.Suit.{Clubs, Diamonds, Hearts, Spades}
import cats.implicits.*
import core.game.Player.*
import core.game.Team.teamA
import core.game.bidding.Contract
import core.game.roundTree.GameTreeNode

import scala.math.BigDecimal.RoundingMode

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      shuffledDeck = Deck.shuffled

      game = Game
        .init(
          Suit.None,
          shuffledDeck,
          TwoThreeThree
        )
        .copy(trumpSuit = Spades)

      /*hMap = HandMap.fromStrings(
        "Js,Qs,Ac,Td,Tc,Qd,Jc,9d",
        "Ks,8s,Th,Kd,Qh,8d,7d,7h",
        "9s,Ts,Ah,Ad,Qc,Jd,9h,7c",
        "As,7s,Kh,Kc,Jh,9c,8h,8c"
      )*/

      hMap = HandMap.fromStrings(
        "Js,Qs,Ac,Td,Tc,Qd,Jc,9d",
        "Ks,8s,Th,Kd,Qh,8d,7d,7h",
        "9s,Ts,Ah,Ad,Qc,Jd,9h,7c",
        "As,7s,Kh,Kc,Jh,9c,8h,8c"
      )

      _ <- hMap.printInfo(Spades)

      randomGame = GameV2("aze", hMap, Spades)
      res = randomGame.generateRandomGameWithFixedHandMap(
        randomGame.initialGameState,
        Map(
          player1 -> List.empty[Card],
          player2 -> List.empty[Card],
          player3 -> List.empty[Card],
          player4 -> List.empty[Card]
        )
      )

      tricks = GameV2.computeTricksFromGameStates(res).get

      _ <- IO.println(tricks.map(_.print).mkString("\n"))

      _ <- IO.never

      res = hMap.genNewHandMapWithForbidden(
        player1,
        Map(
          player2 -> Spades.generateCards.++(Hearts.generateCards),
          player3 -> Spades.generateCards,
          player4 -> Nil
        )
      )

      resL = List.fill(1000000)(
        hMap.genNewHandMapWithForbidden(
          player1,
          Map(
            player2 -> Spades.generateCards.++(Hearts.generateCards),
            player3 -> Spades.generateCards,
            player4 -> Nil
          )
        )
      )

      /*x = res + (player1 -> hMap.getPlayerCards(player1))
      y = HandMap.fromMap(x)

      _ <- IO.println(y)
      /*
      _ <- hMap.printInfo(Spades)
      _ <- hMap.randomExceptPlayer(player1).printInfo(Spades)
      _ <- hMap.randomExceptPlayer(player2).printInfo(Spades)*/

      /*results = hMap
        .optimizeValuesforPlayerHand(player1, 100, 1000)
        .filter(_.trumpSuit.==(Spades))*/

      handMap = HandMap.randomHand

      _ <- hMap.printInfo(Spades)
      _ <- IO.println("\n")

      (optGame, optData) = Game
        .fromHandMap(hMap, Spades)
        .generateRandomOptGame(10000)
        .get

      _ <- IO.println(optData.asJson.spaces4)

      // _ <- optGame.printTricks
      // _ <- optGame.printPoints

      _ <- IO.never

      /*results2 = List
        .fill(10)(HandMap.randomHand)
        .map(hMap => {
          hMap
        })
        .map(_.getBestExpectedResultTeamA(10000))*/

      /*evs = Contract.evFromSimulations(results2, teamA)

      _ <- IO.println(
        evs.mkString("\n")
      )*/

      /*evs = Contract.values.toList.map(contract => {
        val outcomes =
          results.map(res => Contract.pointsMade(res, teamA, contract))
        val ev = outcomes.sum / outcomes.length
        (contract, ev)
      })*/

      xx = results
        .map(res => (res.cardsPlayedOrdered.head.getNotation, res.pointsA))
        .groupBy(_._1)
        .toList
        .map((x, l) => (x, l.length, l.map(_._2).sum / l.length))
        .sortBy(_._2)
        .reverse

      firstCards = results
        .map(_.cardsPlayedOrdered.head.getNotation)
        .groupBy(x => x)
        .toList
        .map((x, l) => (x, l.length))
        .sortBy(_._2)
        .reverse*/

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
