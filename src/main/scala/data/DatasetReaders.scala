package data

import cats.effect.IO
import core.game.GameV2.GameResultStr
import core.game.cards.neutral.HandNeutral
import core.game.cards.{Card, Hand, Suit}
import fs2.io.file.{Files, Path}
import io.circe.Json
import io.circe.generic.auto.*
import fs2.data.json.*
import fs2.data.json.circe.*

import scala.concurrent.duration.*

object DatasetReaders {
  def readComputedOptGames = {
    val jsonStream = Files[IO]
      .list(Path("datasets/computedOptGames"))
      .flatMap(path => Files[IO].readUtf8Lines(path))
      // .metered(1000.millis)
      // .map(s => if (s == "}") "}\n" else s)
      .map(_.stripMargin)
      .through(ast.parse)
    val resStream = jsonStream
      .map(_.as[GameResultStr])
      .collect { case Right(value) =>
        value
      }
    /*resStream
      .map(res => (res.cards1, res.trumpSuit))
      .map((cards, trumpSuit) =>
        (
          Hand(cards.split(",").map(Card.fromLitteral).toList),
          Suit.fromString(trumpSuit)
        )
      )
      .map((hand, trumpSuit) => {
        val handNeutral = HandNeutral.fromHand(hand, trumpSuit)
        s"${hand.toStringTrumpOrdered(trumpSuit).mkString(",")} -> ${handNeutral.toString}"
      })*/
    resStream
    // .debug()
  }
}
