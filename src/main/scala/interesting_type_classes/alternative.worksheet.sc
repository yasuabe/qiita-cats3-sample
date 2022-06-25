import cats.Alternative
import cats.syntax.apply.*
import cats.syntax.semigroupk.*
import cats.syntax.functor.*
import cats.syntax.option.*
import cats.instances.option.*
import cats.syntax.traverse.*
import cats.instances.list.*

trait Div[A] extends Function1[Int, Option[A]]:
  def apply(n: Int): Option[A]

object Div:
  def apply(m: Int, s: String): Div[String] = _.some.filter(_ % m == 0).as(s)
  val Default:                  Div[String] = _.toString.some

given Alternative[Div] with
  def empty[A]: Div[A]                              = _ => None
  def combineK[A](x: Div[A], y: Div[A]): Div[A]     = in => x(in) orElse y(in)
  def pure[A](x: A): Div[A]                         = _ => x.some
  def ap[A, B](ff: Div[A => B])(fa: Div[A]): Div[B] = in => ff(in) <*> fa(in)

val fizzbuzz = Div(15, "FizzBuzz") <+> Div(3, "Fizz") <+> Div(5, "Buzz") <+> Div.Default

(1 to 100).map(fizzbuzz).toList.sequence
