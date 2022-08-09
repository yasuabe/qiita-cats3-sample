import cats.syntax.option.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
case class Movie(id: Int, title: String)

trait MovieRepoComponent:
  trait MovieRepo:
    def getMovie(id: Int): IO[Option[Movie]]

trait MovieServiceComponent { this: MovieRepoComponent  =>
  val movieRepo: MovieRepo

  class MovieService:
    def getMovie(id: Int): IO[Option[Movie]] = movieRepo.getMovie(id)
}
// application layer -------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))

trait MovieRepoComponentImpl extends MovieRepoComponent:
  class MovieRepoImpl extends MovieRepo:
    def getMovie(id: Int): IO[Option[Movie]] = IO(db.get(id))

object Registry
    extends MovieServiceComponent with MovieRepoComponentImpl:

  val movieRepo    = new MovieRepoImpl
  val movieService = new MovieService

val program = Registry.movieService.getMovie(42)

// rumtime layer -------------------------------------------------
program.unsafeRunSync()

// test environment -------------------------------------------------------
trait MovieRepoComponentTestImpl extends MovieRepoComponent:
  class MovieRepoTestImpl extends MovieRepo:
    def getMovie(id: Int): IO[Option[Movie]] = IO(Movie(-1, "Test").some)

object TestRegistry
    extends MovieServiceComponent with MovieRepoComponentTestImpl:

  val movieRepo    = new MovieRepoTestImpl
  val movieService = new MovieService

val testProgram = TestRegistry.movieService.getMovie(42)
testProgram.unsafeRunSync()
