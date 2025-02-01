import cats.effect.{ExitCode, IO, IOApp}
import core.game.cards.Suit.Hearts
import core.game.cards.{Card, Hand, Suit}
import core.game.cards.neutral.HandNeutral
import data.DatasetReaders
import data.analysis.StatsByNeutralHand
import data.reformating.ReformatingGameResults

object PerformanceTests extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      /*testSample: List[List[String]] = List(
        List("Th", "Kh", "Qh", "Jh", "9h", "8h", "7h"),
        List("Ac"),
        List("Ac", "8c"),
        List("Td", "Kd"),
        List("Td", "Kd", "7d")
      )

      testSampleSorted = HandNeutral.sortColors(
        testSample.map(_.map(Card.fromLitteral))
      )

      _ <- IO.println(
        testSampleSorted.map(_.map(_.getNotation).mkString(",")).mkString("\n")
      )

      handS = "Ts,Ad,Ah,Td,Jd,Jh,9h,8h"
      hand = Hand(handS.split(",").map(Card.fromLitteral).toList)
      handNeutral = HandNeutral.fromHand(hand, Hearts)

      _ <- IO.println(handNeutral.cards.map(_.getNotation).mkString(","))*/

      stream = DatasetReaders.readComputedOptGames // .take(1000)

      _ <- stream.compile.drain

      res <- ReformatingGameResults.azerty(stream)

      _ <- IO.println(res.mkString("\n"))

      _ <- IO.never

      analysisStream = StatsByNeutralHand.fromDatasetStream(stream)

      /*res <- analysisStream.compile.toList
        .map(_.sortBy(_._1.pointsInHand))
        .map(
          _.map((h, res) =>
            s"${h.toString}: ${res.pointsA}pts (${h.pointsInHand}pts in hand)(" +
              s"cards2: ${HandNeutral.fromHand(
                  Hand.fromString(res.cards2),
                  Suit.fromString(res.trumpSuit)
                )}, cards3: ${HandNeutral.fromHand(
                  Hand.fromString(res.cards3),
                  Suit.fromString(res.trumpSuit)
                )}, cards4: ${HandNeutral.fromHand(
                  Hand.fromString(res.cards4),
                  Suit.fromString(res.trumpSuit)
                )})"
          )
        )*/

      // _ <- IO.println(res.mkString("\n"))

      // _ <- StatsByNeutralHand.pointsPointsInHandWrite(stream)

      dataL <- stream.compile.toList
      res = dataL
        .groupBy(_.id.split("-").reverse.tail)
        .map((key, l) => l.length)
        .toList
        .groupBy(x => x)
        .map((i, l) => (i, l.length))
        .toList
        .sortBy(_._1)

      _ <- IO.println(res.mkString("\n"))

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
