import zio.{ Runtime, IO, ZIO, ZEnvironment, Unsafe }

// domain layer -----------------------------------------------------------
enum AppError:
  case NoValue
import AppError.*

case class Movie(id: Int, title: String)

trait MovieRepo:
  def getMovie(id: Int): IO[AppError, Movie]

// application layer ----------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))
object MovieRepoImpl extends MovieRepo:
  def getMovie(id: Int): IO[AppError, Movie] =
    ZIO.fromEither(db.get(id).toRight(NoValue))

trait Env:
  val movieRepo: MovieRepo

object Env extends Env:
  val movieRepo = MovieRepoImpl

object MovieService:
  def getMovie(id: Int): ZIO[Env, AppError, Movie] =
    ZIO.environment[Env].flatMap(_.get[Env].movieRepo.getMovie(id))

val program: ZIO[Env, AppError, Movie] = MovieService.getMovie(42)

// runtime layer -------------------------------------------------
val runtime = Runtime.default.withEnvironment(ZEnvironment(Env))
Unsafe.unsafe(runtime.unsafe.run(program))


// testing layer -------------------------------------------------
import cats.syntax.either.*
val testEnv: Env = new:
   val movieRepo = _ => ZIO.fromEither(NoValue.asLeft)

val testRuntime = Runtime.default.withEnvironment(ZEnvironment(testEnv))
Unsafe.unsafe(testRuntime.unsafe.run(program))
