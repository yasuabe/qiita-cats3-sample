import cats.{ Id, Monad }
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer ------------------------------------------------------
case class Movie(id: Int, title: String)

trait MovieRepoSym[F[_]]:
  def getMovie(id: Int): F[Option[Movie]]

class MovieService[F[_]: Monad](sym: MovieRepoSym[F]):
  def getMovie(id: Int): F[Option[Movie]] = sym.getMovie(id)

// application layer ------------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))

object IOInterpreter extends MovieRepoSym[IO]:
  def getMovie(id: Int): IO[Option[Movie]] = IO(db.get(id))

val service = MovieService(IOInterpreter)
val program = service.getMovie(42)

// runtime (the end of the universe) -------------------------------------
program.unsafeRunSync()
//Some(Movie(42,A Movie))

// test environment -------------------------------------------------------
object TestInterpreter extends MovieRepoSym[Id]:
  def getMovie(id: Int): Option[Movie] = Option(Movie(-1, "Dummy"))

val id1 = MovieService(TestInterpreter).getMovie(42)
//Some(Movie(-1,Dummy))