package data.reformating

import cats.effect.IO
import core.game.GameV2.GameResultStr
import core.game.cards.Hand
import core.game.cards.Suit.Clubs

object ReformatingGameResults {
  def azerty(stream: fs2.Stream[IO, GameResultStr]) =
    stream
      .groupAdjacentBy(res =>
        Hand.fromString(res.cards1).toStringTrumpOrdered(Clubs).mkString(",")
      )
      .compile
      .toList
      .map(l =>
        l.map((key, value) => (key, value.toList.length))
          .groupBy(_._2)
          .toList
          .map((key, value) => (key, value.length))
      )
}
