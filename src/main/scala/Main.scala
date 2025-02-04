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
import core.game.cards.neutral.HandNeutral
import core.game.result.ResultAllSuits
import core.game.roundTree.{GameTreeNode, Tricks}
import data.DatasetReaders
import data.compute.RandomOptGameComputing
import data.transform.{ToNeutralHandPoints, ToResultAllSuits}
import fs2.io.file.{Files, Path}
import fs2.text

import scala.math.BigDecimal.RoundingMode

object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      // _ <- RandomOptGameComputing.computeAndPersistN(10, 100, start, 1000)

      /*l <- DatasetReaders
        .readResultAllSuits("v01")
        .filter(_.pointsMap.size.==(4))
        /*.map(res =>
          res.pointsMap.toList
            .map((suit, pts) => (res.hand.countPoints(suit), pts))
            .sortBy(_._1)
            .reverse
            .head
            ._2
        )*/
        .map(res => res.pointsMap.toList.map(_._2).max)
        .compile
        .toList

      res = Contract.pointsByContractSummary(l)

      _ <- IO.println(res.mkString("\n"))
       */

      /*dataL <- DatasetReaders
        .readNeutralHandPoints("v01")
        // .filter(_._1.trumps.toString().==("Jt,9t"))
        // .map((hand, pts) => (hand.trumps, pts))
        .compile
        .toList*/

      /*res = dataL
        .groupBy(_._1.cards.length)
        .map((hand, l) => {
          val average = l.map(_._2).sum / l.length
          val stdDeviation =
            Math
              .pow(
                l.map(_._2)
                  .map(pts => Math.pow(pts - average, 2))
                  .sum / l.length,
                0.5
              )
          (hand.toString, average, stdDeviation, l.length)
        })
        .toList
        .sortBy(_._2)
        .map((hand, avg, std, n) =>
          s"[$hand] $avg (average) $std (std) ($n samples)"
        )*/

      /*res = dataL
        .flatMap((hand, pts) => hand.cards.map(card => (card, pts)))
        .groupBy(_._1.toString)
        .toList
        .sortBy((_, l) => l.map(_._2).sum / l.length)
        .map((key, l) => {
          val avgPts = l.map(_._2).sum / l.length
          s"$key, $avgPts (avg) (${l.length} samples, ${(BigDecimal(l.length) / BigDecimal(dataL.length) * 100)
              .setScale(2, RoundingMode.HALF_DOWN)}%)"

        })

      _ <- IO.println(res.mkString("\n"))
       */

      x = HandNeutral.fromNotation("At+,Kt-").map(elt => elt)

      _ <- IO.println(x.map(_.toString).mkString(","))

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
