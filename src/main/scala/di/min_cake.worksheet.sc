import cats.syntax.option.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
case class Movie(id: Int, title: String)

trait MovieRepo:
  def getMovie(id: Int): IO[Option[Movie]]

trait UsesMovieRepo:
  val movieRepo: MovieRepo

trait MovieService extends UsesMovieRepo:
  def getMovie(id: Int): IO[Option[Movie]] = movieRepo.getMovie(id)

// application layer -------------------------------------------------
val dB = Map[Int, Movie](42 -> Movie(42, "A Movie"))
object MovieRepoImpl extends MovieRepo:
  def getMovie(id: Int): IO[Option[Movie]] = IO(dB.get(id))

trait MixInMovieRepo:
  val movieRepo: MovieRepo = MovieRepoImpl

object MovieService extends MovieService with MixInMovieRepo
val program = MovieService.getMovie(42)

// rumtime layer -------------------------------------------------
program.unsafeRunSync()

// test environment -------------------------------------------------------
val testMovieService: MovieService = new:
  val movieRepo: MovieRepo = new:
    def getMovie(id: Int): IO[Option[Movie]] = IO(Movie(-1, "Test").some)

val testProgram = testMovieService.getMovie(42)
testProgram.unsafeRunSync()
