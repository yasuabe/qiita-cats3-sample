import cats.free.Free
import cats.free.Free.liftF
import cats.~>
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
case class Movie(id: Int, title: String)

enum Query[A]:
  case GetMovie(id: Int) extends Query[Option[Movie]]
import Query.*

type QueryF[T] = Free[Query, T]

trait MovieRepo:
  def getMovie(id: Int): QueryF[Option[Movie]]

trait UsesMovieRepo:
  val movieRepo: MovieRepo

trait MovieService extends UsesMovieRepo:
  def getMovie(id: Int): QueryF[Option[Movie]] = movieRepo.getMovie(id)

object MovieRepoImpl extends MovieRepo:
  def getMovie(id: Int): QueryF[Option[Movie]] = liftF(GetMovie(id))

trait MixInMovieRepo:
  val movieRepo: MovieRepo = MovieRepoImpl

object MovieService extends MovieService with MixInMovieRepo

// application layer -------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))

def ioInterpreter: Query ~> IO = new:
  def apply[A](fa: Query[A]): IO[A] = fa match
    case GetMovie(id) => IO(db.get(id))

val program = MovieService.getMovie(42).foldMap(ioInterpreter)

// runtime  ------------------------------------------------------
program.unsafeRunSync()
