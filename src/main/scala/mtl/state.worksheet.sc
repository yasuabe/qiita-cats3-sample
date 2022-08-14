import cats.Monad
import cats.data.StateT
import cats.effect.{ IO, Sync }
import cats.mtl.Stateful
import cats.syntax.show.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import cats.syntax.traverse.*

type Cache = Map[Int, Int]

def triple[F[_]: Sync](n: Int): F[Int] =
  Sync[F].delay { println(show"triple($n)") } as (n * 3)

def tripleWithCache[F[_]: Sync](n: Int)(using F: Stateful[F, Cache]): F[Int] =
  for
    memo  <- F.get
    value <- memo.get(n)
      .fold(triple[F](n) >>= { v => F.modify(_.updated(n, v)) as v })(_.pure[F])
  yield value

import cats.effect.unsafe.implicits.*

List(1, 3, 1, 2, 3)
  .traverse(tripleWithCache[StateT[IO, Cache, *]])
  .run(Map(3 -> 9))
  .unsafeRunSync()
// triple(1)
// triple(2)
// (Map(3 -> 9, 1 -> 3, 2 -> 6), List(3, 9, 3, 6, 9))


