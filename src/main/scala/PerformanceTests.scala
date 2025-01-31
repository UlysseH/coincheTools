import cats.effect.{ExitCode, IO, IOApp}
import core.game.cards.Suit.Hearts
import core.game.cards.{Card, Hand}
import core.game.cards.neutral.HandNeutral
import data.DatasetReaders

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

      stream = DatasetReaders.readComputedOptGames

      _ <- stream.compile.drain

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
