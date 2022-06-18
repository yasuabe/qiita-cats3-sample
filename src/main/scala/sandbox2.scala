import scala.util.Try
import cats.syntax.option.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import cats.syntax.apply.*
import algebra.ring.Field
import algebra.ring.EuclideanRing

case class Operator(s: String, f: (Double, Double) => Double) extends Function2[Double, Double, Option[Double]]:
  override def toString(): String = s
  def apply(d: Double, e: Double): Option[Double] = Try(f(d, e)).toOption

object Operator:
  val all = List[(String, (Double, Double) => Double)](
    ("+", _ + _),
    ("-", _ - _),
    ("*", _ * _),
    ("/", _ / _)).map(Operator.apply)

type Evaluation = (Int, Int, Int, Int, Operator, Operator, Operator) => Option[Double]

case class Expression(fmt: String, calc: Evaluation):
  def judge(ns: List[Int], os: List[Operator]): Option[String] =
    val a :: b :: c :: d :: Nil = ns
    val o :: p :: q :: Nil = os
    calc(a, b, c, d, o, p, q).filter(_ == 48).as(fmt.format(a, o, b, p, c, q, d))

object Expression:
  val all = List[(String, Evaluation)](
    ("(((%d %s %d) %s %d) %s %d)", (a, b, c, d, o, p, q) => o(a, b).flatMap(p(_, c)).flatMap(q(_, d))),
    ("((%d %s (%d %s %d)) %s %d)", (a, b, c, d, o, p, q) => o(b, c).flatMap(p(a, _)).flatMap(q(_, d))),
    ("((%d %s %d) %s (%d %s %d))", (a, b, c, d, o, p, q) => (o(a, b), p(c, d)).mapN(q).flatten),
    ("(%d %s ((%d %s %d) %s %d))", (a, b, c, d, o, p, q) => o(b, c).flatMap(p(_, d)).flatMap(q(a, _))),
    ("(%d %s (%d %s (%d %s %d)))", (a, b, c, d, o, p, q) => o(c, d).flatMap(p(b, _)).flatMap(q(a, _)))
  ).map(Expression.apply)

def run = 
  val numsList = List(3, 6, 6, 8).permutations.distinct.toList

  val opsList: List[List[Operator]] = List
    .fill(3)(Operator.all)
    .foldRight(List.empty[Operator].pure[List])((_, _).mapN(_ :: _))

  (Expression.all, numsList, opsList).mapN(_.judge(_, _)).flatten.mkString("\n")

// import cats.Eq
import eu.timepit.refined.refineV
import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.Not
import eu.timepit.refined.generic.Equal
import shapeless.Nat._0
import scala.annotation.tailrec
import scala.math.*
import algebra.ring.Field
import algebra.ring.EuclideanRing
// import eu.timepit.refined.{W, refineMV}

type NonZero    = Not[Equal[_0]]
type NonZeroInt = Int Refined NonZero

def gcd_(a: Long, b: Long): Long =
  @tailrec def loop(x: Long, y: Long): Long = if y == 0 then x else loop(y, x % y)
  loop(max(a, b), min(a, b))

case class Rational(n: Long, d: Long)

object Rational:
  def reduced(a: Long, b: Long): Rational =
    val g: Long = gcd_(a, b)
    Rational(a / g, b / g)

given Field[Rational] with
  def zero: Rational = Rational(0, 1)
  def one: Rational = Rational(1, 1)
  def negate(x: Rational): Rational = Rational(- x.n, x.d)
  def plus(x: Rational, y: Rational): Rational =
    Rational.reduced(x.n * y.d + y.n * x.d, x.d * y.d)
  def times(x: Rational, y: Rational): Rational =
    Rational.reduced(x.n * y.n, x.d * y.d)
  def div(x: Rational, y: Rational): Rational =
    Rational.reduced(x.n * y.d, x.d * y.n)