import cats.{ Applicative, Monad }
import cats.data.ReaderT
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.IO
import cats.mtl.{ Ask, Local }

type Config = Map[String, String]

def get42[F[_]: Applicative](using F: Ask[F, Config]): F[String] =
  F.ask.map(_.getOrElse("42", "none"))

def both[F[_]: Monad](using L: Local[F, Config]): F[(String, String)] =
  for
    m <- L.local(get42[F])(_.updated("42", "modified"))
    o <- get42[F]
  yield (m, o)

import cats.effect.unsafe.implicits.*

both[ReaderT[IO, Config, *]].run(Map("42" -> "original")).unsafeRunSync()
//  (modified,original)
