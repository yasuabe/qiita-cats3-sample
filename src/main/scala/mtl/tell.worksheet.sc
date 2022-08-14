// https://qiita.com/yasuabe2613/items/103c0b859ec3bf0f8c1c
import cats.FlatMap
import cats.data.{ Chain, WriterT }
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.mtl.Tell
import cats.effect.{ IO, Sync }
import Chain.one

type Logs = Chain[String]

def tellFunc[F[_]](n: Int)(using F: Tell[F, Logs], S: Sync[F]): F[Unit] =
  for
    _ <- F.tell(one("begin"))
    _ <- S.delay { println(s"n = $n") }
    _ <- F.tell(one("end"))
  yield ()

def program: IO[Logs] = tellFunc[WriterT[IO, Logs, *]](42).written

import cats.effect.unsafe.implicits.*
program.unsafeRunSync()
// n = 42
// res0: Logs = Chain(begin, end)
1