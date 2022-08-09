import cats.free.Free.liftF
import cats.free.Free
import cats.{Id, ~>}
import cats.effect.IO
import cats.effect.unsafe.implicits.*

// domain layer -----------------------------------------------------------
case class Movie(id: Int, title: String)

enum Query[A]:
  case GetMovie(id: Int) extends Query[Option[Movie]]

import Query.*

type QueryF[T] = Free[Query, T]

object MovieRepoOps:
  def getMovie(id: Int): QueryF[Option[Movie]] = liftF(GetMovie(id))

object MovieService:
  def getMovie(id: Int): QueryF[Option[Movie]] = MovieRepoOps.getMovie(id)

// application layer ------------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))

def taskInterpreter: Query ~> IO = new:
  def apply[A](fa: Query[A]): IO[A] = fa match
    case GetMovie(id) => IO(db.get(id))

val program = MovieService.getMovie(42).foldMap(taskInterpreter)

// rumtime layer -------------------------------------------------
program.unsafeRunSync()
// Some(Movie(42, A Movie))

// test environment -------------------------------------------------------
def testInterpreter: Query ~> Id = new:
  def apply[A](fa: Query[A]): Id[A] = fa match
    case GetMovie(_: Int) => Option(Movie(-1, "Test"))

val id1 = MovieService.getMovie(42).foldMap(testInterpreter)
// Some(Movie(-1,Test))
