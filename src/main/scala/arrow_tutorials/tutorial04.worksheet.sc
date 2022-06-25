import cats.Semigroup
import cats.arrow.Arrow
import cats.data.Kleisli

sealed case class SimpleFunc[A, B](runF: A => B)

import cats.syntax.arrow.*
import cats.syntax.compose.*
import cats.syntax.flatMap.*
import cats.syntax.semigroup.*

def arr[F[_, _]: Arrow, A, B](f: A => B): F[A, B] = Arrow[F] lift f
def split[F[_, _]: Arrow, A]: F[A, (A, A)] = arr(x => (x, x))
def unsplit[F[_, _]: Arrow, A, B, C](f: (A, B) => C): F[(A, B), C] =
  arr(f.tupled)
def liftA2[F[_, _], A, B, C, D](f: (B, C) => D, fb: F[A, B], fc: F[A, C])(using Arrow[F]): F[A, D] =
  (fb &&& fc) >>> unsplit(f)
//  split[F, A] >>> A.first(fb) >>> A.second(fc) >>> unsplit(f)

// 6. A Teaser
type ListKleisli[A, B] = Kleisli[List, A, B]

def returnA[S] = Arrow[ListKleisli].id[S]

extension [X[_], S](k1: Kleisli[X, S, S])(using M: Semigroup[X[S]])
  def <+>(k2: Kleisli[X, S, S]) = Kleisli((s: S) => k1.run(s) |+| k2.run(s))

val arrK = f => arr[ListKleisli, String, String](f)

val prepend = (s: String)                      => arrK(s ++ _)
val append  = (s: String)                      => arrK(_ ++ s)
val withId  = (t: ListKleisli[String, String]) => returnA[String] <+> t

val xform = withId(prepend("<"))
        >>> withId(append(">"))
        >>> withId(prepend("!")
        >>> append("!"))

List("test", "foo") >>= xform.run
