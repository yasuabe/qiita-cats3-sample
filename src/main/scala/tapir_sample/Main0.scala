package tapir_sample

import cats.syntax.either.*
import cats.syntax.applicative.*
import cats.effect.{ IO, IOApp }
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{ HttpApp, HttpRoutes }
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

object Main0 extends IOApp.Simple:

  val helloWorldEP: PublicEndpoint[String, Unit, String, Any] =
    endpoint.get.in("hello" / path[String]("name")).out(stringBody)

  def helloLogic(name: String): IO[Either[Unit, String]] =
    s"Hello, $name.".asRight[Unit].pure[IO]

  val helloWorldRoute: HttpRoutes[IO] =
    Http4sServerInterpreter[IO]().toRoutes(helloWorldEP serverLogic helloLogic)

  val helloWorldService: HttpApp[IO] =
    helloWorldRoute.orNotFound

  def run: IO[Unit] = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(helloWorldService)
    .build
    .use(_ => IO.never)
    .void
