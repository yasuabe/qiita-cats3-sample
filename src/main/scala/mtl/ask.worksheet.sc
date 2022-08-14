import cats.FlatMap
import cats.data.Kleisli
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.effect.Sync
import cats.effect.IO
import cats.mtl.Ask

type Config = Map[String, String]

def askFunc[F[_]](key: String)(using F: Ask[F, Config], S: Sync[F]): F[String] =
  for
    config <- F.ask
    result =  config.getOrElse(key, "none")
    _      <- S.delay { println(s"$key -> $result") }
  yield result

import cats.syntax.traverse.*

val program: IO[List[String]] =
  List("21", "42", "63")
    .traverse(askFunc[Kleisli[IO, Config, *]])
    .run(Map("42" -> "foo"))

import cats.effect.unsafe.implicits.*

program.unsafeRunSync()
// 21 -> none
// 42 -> foo
// 63 -> none
// res0: List[String] = List(none, foo, none)
