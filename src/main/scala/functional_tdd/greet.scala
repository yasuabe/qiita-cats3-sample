package functional_tdd

import java.time.LocalTime
import cats.Functor
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.effect.{ ExitCode, IO, IOApp }

// https://qiita.com/yasuabe2613/items/fbdd139f00161ff3186d

trait Now[F[_]]:
  def time: F[LocalTime]

object Now:
  def apply[F[_]](using N: Now[F]) = N

object NowDemo extends IOApp.Simple:
  given Now[IO] with
    def time: IO[LocalTime] = IO.delay(LocalTime.now)

  def run: IO[Unit] = Now[IO].time >>= IO.println

object greet:
  // 参照等価な部分
  def greetingOf(t: LocalTime): String =
    val before = (h: Int, m: Int, s: Int) => t isBefore LocalTime.of(h, m, s)

    if      before( 5, 0, 0) then "こんばんは"
    else if before(12, 0, 0) then "おはようございます"
    else if before(18, 0, 0) then "こんにちは"
    else                          "こんばんは"

  // 参照等価な greetingOf と、参照透過じゃない Now.time の合成
  def greetNow[F[_]: Functor: Now]: F[String] =
    Now[F].time map greetingOf

object GreeterDemo extends IOApp.Simple:
  import greet.*

  given Now[IO] with
    def time: IO[LocalTime] = IO.delay(LocalTime.now)

  def run: IO[Unit] = 
    ((Now[IO].time map greetingOf) >>= IO.println) >>
    (greetNow[IO]                  >>= IO.println)
