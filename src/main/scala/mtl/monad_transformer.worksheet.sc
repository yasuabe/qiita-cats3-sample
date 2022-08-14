import cats.data.{ EitherT, State, StateT }

object A:
  type SET[E, S, R] = StateT[Either[E, *], S, R]
  type EST[E, S, R] = EitherT[State[S, *], E, R]

  def decrementSE: SET[String, Int, Unit] = for
    x <-               StateT.get[Either[String, *], Int]
    _ <- if x > 0 then StateT.set[Either[String, *], Int](x - 1)
                  else StateT.liftF[Either[String, *], Int, Unit](Left("error"))
  yield ()

  def decrementES: EST[String, Int, Unit] = for
    x <-               EitherT.liftF[State[Int, *], String, Int](State.get[Int])
    _ <- if x > 0 then EitherT.liftF[State[Int, *], String, Unit](State.set(x - 1))
                  else EitherT.leftT[State[Int, *], Unit]("error")
  yield ()

  def runSE(n: Int): Either[String, (Int, Unit)] = decrementSE.run(n)
  def runES(n: Int): (Int, Either[String, Unit]) = decrementES.value.run(n).value

A.runSE(0) // Left(error)
A.runSE(1) // Right((0,()))
A.runES(0) // (0,Left(error))
A.runES(1) // (0,Right(()))

