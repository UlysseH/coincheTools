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
import core.game.roundTree.{GameTreeNode, Tricks}

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
        "Js,9s,Qs,8s,Tc,Qd,Jc,9d",
        "Ks,Ac,Th,Kd,Qh,8d,7h,7d",
        "Ts,Ad,Ah,Td,Jd,Jh,9h,8h",
        "As,7s,Kc,Kh,Qc,9c,8c,7c"
      )

      _ <- hMap.printInfo(Spades)

      initialForbiddenHandMap = Map(
        player1 -> List.empty[Card],
        player2 -> List.empty[Card],
        player3 -> List.empty[Card],
        player4 -> List.empty[Card]
      )

      randomGame = GameV2("aze", hMap, Spades)
      /* res = randomGame.generateRandomGameWithFixedHandMap(
        randomGame.initialGameState,
        initialForbiddenHandMap
      )*/

      initialGameState = randomGame.initialGameState
      stepOne = initialGameState.computeNextGameState(
        Card.fromLitteral("Js"),
        true
      )
      stepTwo = stepOne.computeNextGameState(
        Card.fromLitteral("Ks"),
        true
      )

      playableCardsStepOne = stepOne.generatePlayableCards.map(_._1.getNotation)
      playableCardsStepTwo = stepTwo.generatePlayableCards.map(_._1.getNotation)

      _ <- IO.println(playableCardsStepOne.mkString(","))
      _ <- IO.println(playableCardsStepTwo.mkString(","))

      res2 = randomGame.optimizeRecFromGameState(
        randomGame.initialGameState,
        initialForbiddenHandMap,
        10000
      )

      tricks = GameV2.computeTricksFromGameState(res2).get

      _ <- IO.println(tricks.map(_.print).mkString("\n"))

      _ <- IO.println(
        s"[result] team1: ${Tricks.computePoints(tricks, player1)} | team2: ${Tricks
            .computePoints(tricks, player2)}"
      )

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
