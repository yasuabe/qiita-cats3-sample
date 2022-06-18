enum KVStoreA[A]:
  case Put[T](key: String, value: T) extends KVStoreA[Unit]
  case Get[T](key: String)           extends KVStoreA[Option[T]]
  case Delete(key: String)           extends KVStoreA[Unit]

import cats.free.Free
import KVStoreA.*

type KVStore[A] = Free[KVStoreA, A]

import cats.free.Free.liftF

// Put returns nothing (i.e. Unit).
def put[T](key: String, value: T): KVStore[Unit] =
  liftF[KVStoreA, Unit](Put[T](key, value))

// Get returns a T value.
def get[T](key: String): KVStore[Option[T]] =
  liftF[KVStoreA, Option[T]](Get[T](key))

// Delete returns nothing (i.e. Unit).
def delete(key: String): KVStore[Unit] =
  liftF(Delete(key))

// Update composes get and set, and returns nothing.
def update[T](key: String, f: T => T): KVStore[Unit] =
  for {
    vMaybe <- get[T](key)
    _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure(()))
  } yield ()

def program: KVStore[Option[Int]] =
  for {
    _ <- put("wild-cats", 2)
    _ <- update[Int]("wild-cats", (_ + 12))
    _ <- put("tame-cats", 5)
    n <- get[Int]("wild-cats")
    _ <- delete("tame-cats")
  } yield n

import cats.arrow.FunctionK
import cats.{Id, ~>}
import scala.collection.mutable

// the program will crash if a key is not found,
// or if a type is incorrectly specified.
def impureCompiler: KVStoreA ~> Id  =
  new (KVStoreA ~> Id) {
    // a very simple (and imprecise) key-value store
    val kvs = mutable.Map.empty[String, Any]

    def apply[A](fa: KVStoreA[A]): Id[A] = fa match {
      case Put(key, value) =>
        println(s"put($key, $value)")
        kvs(key) = value
        ()
      case Get(key) =>
        println(s"get($key)")
        kvs.get(key).asInstanceOf[A]
      case Delete(key) =>
        println(s"delete($key)")
        kvs.remove(key)
        ()
    }
  }
val result: Option[Int] = program.foldMap(impureCompiler)

import cats.data.State

type KVStoreState[A] = State[Map[String, Any], A]
val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
  def apply[A](fa: KVStoreA[A]): KVStoreState[A] =
    fa match {
      case Put(key, value) => State.modify(_.updated(key, value))
      case Get(key) =>
        State.inspect(_.get(key).asInstanceOf[A])
      case Delete(key) => State.modify(_ - key)
    }
}
val result2: (Map[String, Any], Option[Int]) = program.foldMap(pureCompiler).run(Map.empty).value

import cats.data.EitherK
import cats.{Id, InjectK, ~>}

/* Handles user interaction */
sealed trait Interact[A]
case class Ask(prompt: String) extends Interact[String]
case class Tell(msg: String) extends Interact[Unit]

/* Represents persistence operations */
sealed trait DataOp[A]
case class AddCat(a: String) extends DataOp[Unit]
case class GetAllCats() extends DataOp[List[String]]

sealed trait Data2Op[A]
case class AddCat2(a: String) extends Data2Op[Unit]

type CatsApp[A] = EitherK[DataOp, [X] =>> EitherK[Interact, Data2Op, X], A]

class Interacts[F[_]](implicit I: InjectK[Interact, F]) {
  def tell(msg: String): Free[F, Unit] = Free.liftInject[F](Tell(msg))
  def ask(prompt: String): Free[F, String] = Free.liftInject[F](Ask(prompt))
}

object Interacts {
  implicit def interacts[F[_]](implicit I: InjectK[Interact, F]): Interacts[F] = new Interacts[F]
}

class DataSource[F[_]](implicit I: InjectK[DataOp, F]) {
  def addCat(a: String): Free[F, Unit] = Free.liftInject[F](AddCat(a))
  def getAllCats: Free[F, List[String]] = Free.liftInject[F](GetAllCats())
}

object DataSource {
  implicit def dataSource[F[_]](implicit I: InjectK[DataOp, F]): DataSource[F] = new DataSource[F]
}

class DataSource2[F[_]](implicit I: InjectK[Data2Op, F]) {
  def addCat2(a: String): Free[F, Unit] = Free.liftInject[F](AddCat2(a))
}

object DataSource2 {
  implicit def dataSource[F[_]](implicit I: InjectK[Data2Op, F]): DataSource2[F] = new DataSource2[F]
}

def program2(implicit I : Interacts[CatsApp], D : DataSource[CatsApp], D2 : DataSource2[CatsApp]): Free[CatsApp, Unit] = {

  import I._, D._, D2._

  for {
    cat <- ask("What's the kitty's name?")
    _ <- addCat(cat)
    _ <- addCat2(cat)
    cats <- getAllCats
    _ <- tell(cats.toString)
  } yield ()
}
import scala.io.StdIn
import scala.collection.mutable.ListBuffer
object ConsoleCatsInterpreter extends (Interact ~> Id) {
  def apply[A](i: Interact[A]) = i match {
    case Ask(prompt) =>
      println(prompt)
      "hello"// StdIn.readLine()
    case Tell(msg) =>
      println(msg)
  }
}

object InMemoryDatasourceInterpreter extends (DataOp ~> Id) {

  private[this] val memDataSet = new ListBuffer[String]

  def apply[A](fa: DataOp[A]) = fa match {
    case AddCat(a) => memDataSet.append(a); ()
    case GetAllCats() => memDataSet.toList
  }
}
object InMemoryDatasource2Interpreter extends (Data2Op ~> Id) {

  private[this] val memDataSet = new ListBuffer[String]

  def apply[A](fa: Data2Op[A]) = fa match {
    case AddCat2(a) => memDataSet.append(a); ()
  }
}


val interpreter: CatsApp ~> Id = InMemoryDatasourceInterpreter or (ConsoleCatsInterpreter or InMemoryDatasource2Interpreter)
import DataSource._, Interacts._
val evaled: Unit = program2.foldMap(interpreter)

1 + 1