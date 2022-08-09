import cats.syntax.option.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
import cats.data.ReaderT

case class Movie(id: Int, title: String)

trait MovieRepo:
  def getMovie(id: Int): IO[Option[Movie]]

trait UsesMovieRepo:
  val movieRepo: MovieRepo


// application layer ----------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))
object MovieRepoImpl extends MovieRepo:
  def getMovie(id: Int): IO[Option[Movie]] = IO(db.get(id))

trait MovieServiceEnv extends UsesMovieRepo

object MovieServiceEnv extends MovieServiceEnv:
  val movieRepo = MovieRepoImpl

object MovieService:
  def getMovie(id: Int): ReaderT[IO, MovieServiceEnv, Option[Movie]] =
    ReaderT(_.movieRepo.getMovie(id))

val task1 = MovieService.getMovie(42).run(MovieServiceEnv)

// runtime layer -------------------------------------------------
task1.unsafeRunSync()

// test environment -------------------------------------------------------
object TestEnvironment extends MovieServiceEnv:
  val movieRepo: MovieRepo = new:
    def getMovie(id: Int): IO[Option[Movie]] = IO(Movie(-1, "Test").some)

val task2 = MovieService.getMovie(42).run(TestEnvironment)
task2.unsafeRunSync()

