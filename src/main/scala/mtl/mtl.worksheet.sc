import cats.Monad
import cats.data.{ EitherT, StateT, State }
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.mtl.{ Raise, Stateful, Handle }
import cats.mtl.syntax.raise.*
import Stateful.*
import Raise.raise
import Handle.*

type MSI[F[_]] = Stateful[F, Int]
type FRS[F[_]] = Raise[F, String]

def decr[F[_]: Monad: MSI: FRS]: F[Unit] = for
  x <- get
  _ <- if x > 0 then set(x - 1) else raise("error")
yield ()

object M:
  type SET[R] = StateT [Either[String, *], Int   , R]
  type EST[R] = EitherT[State [Int   , *], String, R]

  def runSE(n: Int): Either[String, (Int, Unit)] = decr[SET].run(n)
  def runES(n: Int): (Int, Either[String, Unit]) = decr[EST].value.run(n).value

M.runSE(0) // Left(error)
M.runSE(1) // Right((0,()))
M.runES(0) // (0,Left(error))
M.runES(1) // (0,Right(()))
