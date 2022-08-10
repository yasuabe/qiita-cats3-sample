import cats.free.Free
import cats.free.Free.liftF
import cats.{ Id, Monad, ~> }
import cats.syntax.option.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
case class Movie(id: Int, title: String)

enum Query[A]:
  case GetMovie(id: Int) extends Query[Option[Movie]]
import Query.*

type QueryF[T] = Free[Query, T]

trait MovieRepoAlg[F[_]]:
  def getMovie(id: Int): F[Option[Movie]]

class MovieService[F[_]: Monad](alg: MovieRepoAlg[F]):
  def getMovie(id: Int): F[Option[Movie]] = alg.getMovie(id)

object QueryFInterpreter extends MovieRepoAlg[QueryF]:
  def getMovie(id: Int): QueryF[Option[Movie]] = liftF(GetMovie(id))

val MovieServiceImpl = MovieService(QueryFInterpreter)

// application layer -------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))

def ioInterpreter: Query ~> IO = new:
  def apply[A](fa: Query[A]): IO[A] = fa match
    case GetMovie(id) => IO(db.get(id))

val program = MovieServiceImpl.getMovie(42).foldMap(ioInterpreter)

// runtime  ------------------------------------------------------
program.unsafeRunSync()
//res0: Option[Movie] = Some(Movie(42,A Movie))

// test environment -------------------------------------------------------
val testMovieRepoImpl: MovieRepoAlg[Id] = new:
  def getMovie(id: Int): Option[Movie] = Movie(id, s"Movie($id)").some

val testMovieServiceImpl = MovieService(testMovieRepoImpl)

testMovieServiceImpl.getMovie(42)
// Some(Movie(42,Movie(42)))
