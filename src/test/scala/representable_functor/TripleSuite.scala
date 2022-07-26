package representable_functor

import cats.instances.AllInstances
import cats.laws.discipline.RepresentableTests
import cats.{ Apply, Functor, Representable }
import org.scalacheck.{ Arbitrary, Gen }
import cats.syntax.apply.*
import munit.DisciplineSuite

type Triple[A] = (A, A, A)

given Functor[Triple] with
  def map[A, B](fa: Triple[A])(f: A => B): Triple[B] = (f(fa._1), f(fa._2), f(fa._3))

given Representable.Aux[Triple, Int] = new Representable[Triple]:
  type Representation = Int
  def  F              = Functor[Triple]

  def index[A](f: Triple[A]): Int => A = n =>
    if      n < 0 then f._1
    else if n > 0 then f._3
    else               f._2

  def tabulate[A](f: Int => A): Triple[A] = F.map(-1, 0, 1)(f)

class TripleSpec extends DisciplineSuite:
  given Apply[Gen] with
    def ap[A, B](ff: Gen[A => B])(fa: Gen[A]): Gen[B] = ff flatMap fa.map
    def map[A, B](fa: Gen[A])(f: A => B):      Gen[B] = fa map f

  given Arbitrary[Triple[Int]] = Arbitrary {
    val smallInt = Gen.chooseNum(-2, 2)
    (smallInt, smallInt, smallInt).mapN((_, _, _))
  }
  given Arbitrary[Int => Char] = Arbitrary {
    Gen.oneOf((' ', ' ', ' '), ('a', 'b', 'c'), ('-', '0', '+')) map { case (x, y, z) =>
      (n: Int) => if n < 0 then x else if n > 0 then y else z
    }
  }
  checkAll("Representablity of Triple", RepresentableTests[Triple, Int].representable[Char])
