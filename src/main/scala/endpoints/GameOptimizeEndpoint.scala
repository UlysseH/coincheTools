package endpoints

import cats.effect.{GenSpawn, IO}
import core.game.{Game, Player}
import core.game.cards.{Hand, HandMap, Suit}
import org.http4s.HttpRoutes
import org.http4s.dsl.Http4sDsl
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.circe.*
import io.circe.generic.auto.*
import io.circe.syntax.*

object GameOptimizeEndpoint extends Http4sDsl[IO] {
  private final case class GenerateHandMapPayload(cards: String)
  private final case class GameOptPayload(
      cards1: String,
      cards2: String,
      cards3: String,
      cards4: String,
      trumpSuit: String,
      precision: Int
  )
  def endpoints: HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case GET -> Root / "hello" => Ok("hello")
      //      case GET -> Root / "range" =>
      //        Ok(ranges.map(r => r.name -> r.hands.map(_.getNotation)).toMap)

      case req @ GET -> Root / "generateHandFromPlayer" =>
        for {
          start <- IO(System.currentTimeMillis())
          handMapPayload <- req
            .as[GenerateHandMapPayload]
            .onError(t => IO.println(t))
          res = Hand
            .fromString(handMapPayload.cards)
            .generateRandomHandMap
            .toStringMap
          resp <- Ok(res)
          end <- IO(System.currentTimeMillis())
          _ <- IO.println(s"[computed in] ${end - start}ms")
        } yield resp

      case req @ GET -> Root / "optGameAnalysis" =>
        for {
          start <- IO(System.currentTimeMillis())
          optGamePayload <- req.as[GameOptPayload].onError(t => IO.println(t))
          optGame = Game
            .fromHandMap(
              HandMap.fromStrings(
                optGamePayload.cards1,
                optGamePayload.cards2,
                optGamePayload.cards3,
                optGamePayload.cards4
              ),
              Suit.fromString(optGamePayload.trumpSuit)
            )
          _ <- optGame.printInfo
          (optResult, optData) = optGame
            .generateRandomOptGame(optGamePayload.precision)
            .get
          _ <- optResult.printPoints
          _ <- optResult.printTricks
          resp <- Ok(optData)
          end <- IO(System.currentTimeMillis())
          _ <- IO.println(s"[computed in] ${end - start}ms")
        } yield resp
    }
}
