import cats.Applicative
import cats.data.OptionT
import cats.syntax.option._
import cats.syntax.applicative._

def pure[F[_]]: Applicative[F] ?=> [A] => A => OptionT[F, A] =
  [A] => (a: A) => OptionT(a.some.pure)

pure[List](1)
pure[Option]("a"): OptionT[Option, String]

