import cats.Applicative
import cats.data.OptionT
import cats.syntax.option._
import cats.syntax.applicative._
import scala.annotation.tailrec

def pure[F[_]]: Applicative[F] ?=> [A] => A => OptionT[F, A] =
  [A] => (a: A) => OptionT(a.some.pure)

pure[List](1)
pure[Option]("a"): OptionT[Option, String]


def sample1[A, B, C](list: List[A], f: A => Either[B, C]): Either[B, List[C]] = {
  list.foldLeft[Either[B, List[C]]](Right(Nil)) {
    case (Right(acc), x) => f(x).map(_ :: acc)
    case (l, _)          => l
  }.map(_.reverse)
}
val f1: Int => Either[String, Double] = (n: Int) =>
  assert(n < 6)
  Either.cond(n != 3, n / 2.0, "error") 

sample1(List(2, 4), f1)
sample1(List(1, 2, 3, 4, 5, 6), f1)

def sample2[A, B, C](list: List[A], f: A => Either[B, C]): Either[B, List[C]] = {
  @tailrec
  def rec(acc: List[C], xs: List[A]): Either[B, List[C]] = xs match {
    case x :: tail =>
      f(x) match {
        case Right(r) => rec(r :: acc, tail)
        case Left(l)  => Left(l)
      }
    case Nil =>
      Right(acc)
  }
  rec(Nil, list).map(_.reverse)
}
sample2(List(2, 4), f1)
sample2(List(1, 2, 3, 4, 5, 6), f1)

import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.bifunctor.*
import cats.syntax.bifoldable.*
import cats.ApplicativeError
import cats.Traverse
import cats.FlatMap
import cats.Monad
import cats.Id
import cats.TraverseFilter
import cats.instances.list

def sample4[A, B, C](list: List[A], f: A => Either[B, C]): Either[B, List[C]] =
  list.traverse(f)

sample4(List(2, 5), f1)
sample4(List(1, 2, 3, 4, 5), f1)

val list2: List[Int => Int] = List(_ + 1, _ + 3, _ => throw AssertionError("error"))
val f2: (Int => Int) => Either[Int, Int] = f => if f(0) == 3 then 100.asLeft else f(10).asRight
sample4(list2, f2)

def sample5[A, B, C](list: LazyList[A], f: A => Either[B, C]): Either[B, List[C]] =
  val (rs, ls) = (list map f).span(_.isRight)
  ls.headOption.fold(rs.toList.sequence)(_ as Nil)

val ll = LazyList.from(0).take(2)
val f6: Int => Either[String, Double] = (n: Int) => if n == 3 then "error".asLeft else (n + .5).asRight
sample5(LazyList.from(0).take(2), f6)
sample5(LazyList.from(0).take(5), f6)

def sample6[A, B, C](list: List[A], f: A => Either[B, C]): Either[B, List[C]] =
  summon[FlatMap[Id]].tailRecM((list, Nil)){ (_: (List[A], List[C])) match
    case (a :: as, out) => f(a).swap.bimap(c => (as, c :: out), _.asLeft)
    case (_,       out) => out.reverse.asRight.asRight
  }
val list: List[Int => Int] = List(_ + 1, _ + 2)
val f: (Int => Int) => Either[Int, Int] = f => if f(0) == 3 then 100.asLeft else f(10).asRight
sample6(list, f)
sample6(List[Int=>Int](_ + 1, _ + 3, _ => throw AssertionError("test")), f)
sample6(List(0, 1), f6)
sample6(List(0, 1, 2, 3, 4), f6)

val f4 = (n: Int) => if n > 1 then "strig".asLeft[Int] else (n + 1).asRight[String]
LazyList.from(0).map(f4).takeWhile(_.isRight).toList