package data.analysis

import cats.effect.IO
import core.game.GameV2.GameResultStr
import core.game.cards.{Card, Hand, Suit}
import core.game.cards.neutral.HandNeutral
import cats.kernel.Eq
import cats.syntax.all.*
import fs2.io.file.{Files, Path}

case class StatsByNeutralHand(
    handNeutral: HandNeutral,
    points: List[Int]
)

object StatsByNeutralHand {
  implicit val eqHandNeutral: Eq[HandNeutral] = Eq.fromUniversalEquals
  def fromDatasetStream(data: fs2.Stream[IO, GameResultStr]) = data
    .map(res => (res.cards1, res.trumpSuit, res))
    .map((cards, trumpSuit, res) =>
      (
        Hand(cards.split(",").map(Card.fromLitteral).toList),
        Suit.fromString(trumpSuit),
        res
      )
    )
    .map((hand, trumpSuit, res) => {
      val handNeutral = HandNeutral.fromHand(hand, trumpSuit)
      s"${hand.toStringTrumpOrdered(trumpSuit).mkString(",")} -> ${handNeutral.toString}"
      (handNeutral, res)
    })
  /*.groupAdjacentBy(_._1)
    .map((hand, chunk) => (hand, chunk.toList.map(_._2)))*/
  /*.compile
    .toList*/
  /*.map(
      _.groupBy(_._1).toList
        .map((h, l) => (h, l.flatMap(_._2)))
        // .sortBy((_, l) => l.sum / l.length)
        .sortBy(_._1.pointsInHand)
        .map((h, l) =>
          val points = l.map(_.pointsA)
          s"${h.toString}: ${points.sum / l.length}pts average (${h.pointsInHand}pts in hand)(${l.length} samples)"
        )
    )*/

  def pointsPointsInHandWrite(data: fs2.Stream[IO, GameResultStr]) =
    data
      .map(stats =>
        (
          stats.pointsA,
          Hand
            .fromString(stats.cards1)
            .countPoints(Suit.fromString(stats.trumpSuit))
        )
      )
      .map((ptsA, ptsH) => s"$ptsA,$ptsH")
      .through(
        Files[IO].writeUtf8Lines(
          Path(
            s"datasets/pointsAnalysis/pointsPointsInHand_${System.currentTimeMillis()}.csv"
          )
        )
      )
      .compile
      .drain
}
