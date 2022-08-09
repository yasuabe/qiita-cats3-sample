import cats.data.{ReaderT, Writer}
import cats.free.Free
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

trait Env:
  def movieRepo: MovieRepo

type EnvReader[T] = ReaderT[QueryF, Env, T]

object MovieRepoOps:
  def getMovie(id: Int): QueryF[Option[Movie]] = Free.liftF(GetMovie(id))

object MovieService:
  def getMovie(id: Int): EnvReader[Option[Movie]] =
    ReaderT (_.movieRepo.getMovie(id))

// application layer ------------------------------------------------------
val db = Map[Int, Movie](42 -> Movie(42, "A Movie"))

def taskInterpreter: Query ~> IO = new:
  def apply[A](fa: Query[A]): IO[A] = fa match
    case GetMovie(id) => IO(db.get(id))

val productEnv: Env = new:
  def movieRepo: MovieRepo = id => MovieRepoOps.getMovie(id)

val program = MovieService.getMovie(42)
                          .run(productEnv)
                          .foldMap(taskInterpreter)

// rumtime layer -------------------------------------------------
program.unsafeRunSync() // Some(Movie(42,A Movie))

// test environment --------------------------------------------------------
import cats.syntax.writer.*
import cats.instances.vector.*

type TestWriter[T] = Writer[Vector[Any], T]
def testInterpreter2: Query ~> TestWriter = new:
   def apply[A](q: Query[A]): TestWriter[A] = q match
     case GetMovie(id) => Option(Movie(1, "A Movie")).writer(Vector(q))

val testEnv: Env = new:
  def movieRepo: MovieRepo = _ => MovieRepoOps.getMovie(-1)

val (l, v) = MovieService.getMovie(-1)
                         .run(testEnv)
                         .foldMap(testInterpreter2)
                         .run
// l: Vector[Any] = Vector(GetMovie(-1))
// v: Option[Movie] = Some(Movie(1,A Movie))
