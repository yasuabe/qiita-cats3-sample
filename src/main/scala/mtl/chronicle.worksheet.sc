import cats.Monad
import cats.data.{ Chain, Ior }
import cats.mtl.Chronicle
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

def func[F[_]: Monad](n: Int)(using F: Chronicle[F, Chain[Int]]): F[Int] =
  if      n > 4 then F.confess(Chain(-n))     // Raise#raise 相当
  else if n > 2 then F.dictate(Chain(n)) as n // Tell#tell 相当
                else n.pure[F]

type IorC[A] = Ior[Chain[Int], A]

(1 to 1).toList.traverse(func[IorC]) // Right(List(1))
(1 to 2).toList.traverse(func[IorC]) // Right(List(1, 2))
(1 to 3).toList.traverse(func[IorC]) // Both(Chain(3),List(1, 2, 3))
(1 to 4).toList.traverse(func[IorC]) // Both(Chain(3, 4),List(1, 2, 3, 4))
(1 to 5).toList.traverse(func[IorC]) // Left(Chain(3, 4, -5))
(1 to 6).toList.traverse(func[IorC]) // Left(Chain(3, 4, -5))
