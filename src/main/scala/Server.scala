import cats.effect.{ExitCode, IO, IOApp}
import endpoints.GameOptimizeEndpoint
import org.http4s.blaze.server.BlazeServerBuilder
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
        BlazeServerBuilder[IO](global)
          .bindHttp(8080, "127.0.0.1")
          .withHttpApp(Logger.httpApp[IO](true, true)(httpApp))
          .serve
      }.drain.compile.drain.as(ExitCode.Success)
    } yield exitCode
  }
}
