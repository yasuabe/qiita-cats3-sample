import cats.arrow.Arrow
import cats.data.Kleisli

sealed case class SimpleFunc[A, B](runF: A => B)

import cats.syntax.arrow._
import cats.syntax.compose._

def arr[F[_, _]: Arrow, A, B](f: A => B): F[A, B] = Arrow[F] lift f
def split[F[_, _]: Arrow, A]: F[A, (A, A)] = arr(x => (x, x))
def unsplit[F[_, _]: Arrow, A, B, C](f: (A, B) => C): F[(A, B), C] =
  arr(f.tupled)
def liftA2[F[_, _], A, B, C, D](f: (B, C) => D, fb: F[A, B], fc: F[A, C])(using Arrow[F]): F[A, D] =
  (fb &&& fc) >>> unsplit(f)
//  split[F, A] >>> A.first(fb) >>> A.second(fc) >>> unsplit(f)

//5. Kleisli Arrows 
type Arr = Kleisli[List, Int, Int]
val plusMinus: Arr = Kleisli(x => x :: (-x) :: Nil)
val double   : Arr = arr(_ * 2)
val h2       : Arr = liftA2((_: Int) + (_: Int), plusMinus, double)

h2 run 8