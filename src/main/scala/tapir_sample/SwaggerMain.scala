package tapir_sample

import cats.Semigroup
import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.semigroupk.*
import cats.syntax.applicative.*
import cats.effect.{ IO, IOApp }
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{ HttpApp, HttpRoutes }
import com.comcast.ip4s.*
import sttp.tapir.*
import sttp.tapir.docs.openapi.*
import sttp.tapir.server.http4s.*
import sttp.tapir.server.http4s.Http4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter

object SwaggerMain extends IOApp.Simple:

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

  val swaggerEPs = SwaggerInterpreter().fromEndpoints[IO](
    List(helloEP, hiEP, byeEP),
    "http4s × tapir × Swagger",
    "1.0")

  val swaggerRoutes = Http4sServerInterpreter[IO]().toRoutes(swaggerEPs)

  given Semigroup[HttpRoutes[IO]] = _ combineK _

  extension (ep: PublicEndpoint[String, Unit, String, Any])
    def toRoute(logic: String => IO[Either[Unit, String]]) =
      Http4sServerInterpreter[IO]().toRoutes(ep serverLogic logic)

  val greetingService: HttpApp[IO] = NonEmptyList.of(
    helloEP toRoute hello,
    hiEP    toRoute hi,
    byeEP   toRoute bye,
  ).append(swaggerRoutes)
   .reduce.orNotFound

  def run: IO[Unit] = EmberServerBuilder
    .default[IO]
    .withHost(ipv4"0.0.0.0")
    .withPort(port"8080")
    .withHttpApp(greetingService)
    .build
    .use(_ => IO.never)
    .void