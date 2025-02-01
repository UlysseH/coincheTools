import cats.effect.{ExitCode, IO, IOApp}
import endpoints.GameOptimizeEndpoint
import org.http4s.ember.server._
import com.comcast.ip4s._
import org.http4s.server.Router
import org.http4s.server.middleware.Logger
import org.http4s.implicits.*

import scala.concurrent.ExecutionContext.global

object Server extends IOApp {
  def run(args: List[String]): IO[ExitCode] = {
    for {
      exitCode <- {
        val httpApp =
          Router(
            "/api" -> GameOptimizeEndpoint.endpoints
          ).orNotFound
        EmberServerBuilder
          .default[IO]
          .withHost(ipv4"127.0.0.1")
          .withPort(port"8080")
          .withHttpApp(Logger.httpApp[IO](true, true)(httpApp))
          .build
          .use(_ => IO.never)
          .as(ExitCode.Success)
      }
    } yield exitCode
  }
}
