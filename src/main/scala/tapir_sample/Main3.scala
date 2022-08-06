package tapir_sample

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.semigroupk.*
import cats.syntax.applicative.*
import cats.effect.{ IO, IOApp }
import com.comcast.ip4s.*
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{ HttpApp, HttpRoutes }
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

object Main3 extends IOApp.Simple:

  val nameParam = path[String]("name")

  val greetEP: PublicEndpoint[Unit, Unit, String, Any] =
    endpoint.get.out(stringBody)

  val helloEP: PublicEndpoint[String, Unit, String, Any] =
    greetEP.in("hello" / nameParam)

  val hiEP: PublicEndpoint[String, Unit, String, Any] =
    greetEP.in("hi" / nameParam)

  val byeEP: PublicEndpoint[String, Unit, String, Any] =
    greetEP.in("bye" / nameParam)

  def hello(name: String): IO[Either[Unit, String]] =
    s"Hello, $name".asRight[Unit].pure[IO]

  def hi(name: String): IO[Either[Unit, String]] =
    s"Hi, $name".asRight[Unit].pure[IO]

  def bye(name: String): IO[Either[Unit, String]] =
    s"Bye, $name".asRight[Unit].pure[IO]

  given Semigroup[HttpRoutes[IO]] = _ combineK _

  extension (ep: PublicEndpoint[String, Unit, String, Any])
    def toRoute(logic: String => IO[Either[Unit, String]]) =
      Http4sServerInterpreter[IO]().toRoutes(ep serverLogic logic)

  val greetingService: HttpApp[IO] = NonEmptyList.of(
    helloEP toRoute hello,
    hiEP    toRoute hi,
    byeEP   toRoute bye
  ).reduce.orNotFound

  def run: IO[Unit] = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(greetingService)
    .build
    .use(_ => IO.never)
    .void
