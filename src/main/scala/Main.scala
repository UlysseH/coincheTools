import cats.effect.{ExitCode, IO, IOApp}
import io.circe.generic.auto.*
import io.circe.syntax.*
import core.game.{Game, Player, roundTree}
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

      hMap = HandMap.fromStrings(
        "Js,As,7s,Ts,Qh,9c,8h,8d",
        "Ks,Td,Kd,Jc,Jh,Jd,7h,7d",
        "8s,Ac,Ah,Th,Qd,Qc,8c,7c",
        "9s,Tc,Qs,Ad,Kh,Kc,9d,9h"
      )
      /*
      _ <- hMap.printInfo(Spades)
      _ <- hMap.randomExceptPlayer(player1).printInfo(Spades)
      _ <- hMap.randomExceptPlayer(player2).printInfo(Spades)*/

      /*results = hMap
        .optimizeValuesforPlayerHand(player1, 100, 1000)
        .filter(_.trumpSuit.==(Spades))*/

      handMap = HandMap.randomHand

      _ <- handMap.printInfo(Spades)
      _ <- IO.println("\n")

      optGame = Game
        .fromHandMap(handMap, Spades)
        .generateRandomOptGame(1000)
        .get

      _ <- optGame.printTricks
      _ <- optGame.printPoints

      _ <- IO.never

      results2 = List
        .fill(10)(HandMap.randomHand)
        .map(hMap => {
          hMap
        })
        .map(_.getBestExpectedResultTeamA(100000))

      evs = Contract.evFromSimulations(results2, teamA)

      _ <- IO.println(
        evs.mkString("\n")
      )

      /*evs = Contract.values.toList.map(contract => {
        val outcomes =
          results.map(res => Contract.pointsMade(res, teamA, contract))
        val ev = outcomes.sum / outcomes.length
        (contract, ev)
      })*/

      /*xx = results
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
