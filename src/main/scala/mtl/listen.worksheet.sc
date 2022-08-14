import cats.{ FlatMap, Monad }
import cats.data.{ Chain, WriterT }
import cats.effect.{ IO, Sync }
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.mtl.syntax.listen.*
import cats.mtl.{ Listen, Tell }
import Chain.one

type Logs = Chain[String]

def tellFunc[F[_]](n: Int)(using T: Tell[F, Logs], S: Sync[F]): F[Unit] =
  for
    _ <- T.tell(one("begin"))
    _ <- S.delay { println(s"n = $n") }
    _ <- T.tell(one("end"))
  yield ()

def send[F[_]: Sync](logs: Logs): F[Unit] =
  Sync[F].delay { println(s"Sending to log server: $logs") }

def program[F[_]: Sync: [F[_]] =>> Listen[F, Logs]](n: Int) =
  tellFunc[F](n).listen >>= ((_, logs) => send[F](logs))

import cats.effect.unsafe.implicits.*

program[WriterT[IO, Logs, *]](12345).run.unsafeRunSync()
// n = 12345
// Sending to log server: Chain(begin, end)
// res0: (cats.data.Chain[String], Unit) = (Chain(begin, end),())
