import cats.effect.{ExitCode, IO, IOApp}
import core.game.cards.Card
import core.game.cards.neutral.HandNeutral

object PerformanceTests extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      start <- IO(System.currentTimeMillis())

      testSample: List[List[String]] = List(
        List("Th", "Kh", "Qh", "Jh", "9h", "8h", "7h"),
        List("Ac"),
        List("Ac", "8c"),
        List("Td", "Kd"),
        List("Td", "Kd", "7d")
      )

      testSampleSorted = HandNeutral.sortY(
        testSample.map(_.map(Card.fromLitteral))
      )

      _ <- IO.println(
        testSampleSorted.map(_.map(_.getNotation).mkString(",")).mkString("\n")
      )

      end <- IO(System.currentTimeMillis())
      _ <- IO.println(s"[computed in] ${end - start}ms")
    } yield ExitCode.Success
  }
}
