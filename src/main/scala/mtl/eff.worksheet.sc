import cats.data.State
import org.atnos.eff.*
import org.atnos.eff.all.*
import org.atnos.eff.syntax.all.*

object E:
  type _eitherString[R] = Either[String, *] |= R
  type _stateInt[R]     = State[Int, *]     |= R

  def decr[R: _eitherString: _stateInt]: Eff[R, Unit] = for
    x <- get
    _ <- if x > 0 then put(x - 1) else left("error")
  yield ()

  type ETree = Fx.fx2[State[Int, *], Either[String, *]]

  def runSE(n: Int): Either[String, (Unit, Int)] = decr[ETree].runState(n).runEither.run
  def runES(n: Int): (Either[String, Unit], Int) = decr[ETree].runEither.runState(n).run

E.runSE(0) // Left(error)
E.runSE(1) // Right(((), 0))
E.runES(0) // (Left(error),0)
E.runES(1) // (Right(()),0)

