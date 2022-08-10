import cats.data.Chain
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
case class Movie(id: Int, title: String)

trait MovieRepo[F[_]]:
  def getMovie(id: Int): F[Option[Movie]]

trait UsesMovieRepo[F[_]]:
  val movieRepo: MovieRepo[F]

trait MovieService[F[_]] extends UsesMovieRepo[F]:
  def getMovie(id: Int): F[Option[Movie]] = movieRepo.getMovie(id)

// application layer -------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))
object MovieRepoImpl extends MovieRepo[IO]:
  def getMovie(id: Int): IO[Option[Movie]] = IO(db.get(id))

trait MixInMovieRepo:
  val movieRepo: MovieRepo[IO] = MovieRepoImpl

object MovieService extends MovieService[IO] with MixInMovieRepo
val program: IO[Option[Movie]] = MovieService.getMovie(42)

// runtime  ------------------------------------------------------
program.unsafeRunSync()
//Some(Movie(42,A Movie))

// test environment -------------------------------------------------------
import cats.data.Writer
import cats.syntax.functor.*
import cats.syntax.writer.*
import cats.syntax.option.*

type LogWriter[V] = Writer[Chain[String], V]

val logWriterMovieRepo: MovieRepo[LogWriter] = new:
  def getMovie(id: Int): LogWriter[Option[Movie]] =
    Chain(s"getMovie($id)").tell as Movie(id, "Dummy").some

object TestMovieService extends MovieService[LogWriter]:
  val movieRepo = logWriterMovieRepo

TestMovieService.getMovie(42).run
//(Chain(getMovie(42)),Some(Movie(42,Dummy)))
