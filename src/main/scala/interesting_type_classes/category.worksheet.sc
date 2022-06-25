import cats.arrow.Category
import cats.data.{Chain, Kleisli, Writer}
import cats.syntax.writer._
import cats.syntax.compose._

type ChainWriter[T] = Writer[Chain[String], T]
type KWriter[A, B]  = Kleisli[ChainWriter, A, B]

// given Category[KWriter] with
//   def id[A]: KWriter[A, A] = Kleisli[ChainWriter, A, A] { _.writer(Chain.empty) }
//   def compose[A, B, C](f: KWriter[B, C], g: KWriter[A, B]): KWriter[A, C] = f compose g

def lift[A, B](f: A => B) = Kleisli[ChainWriter, A, B] {
  a => Chain(s"$a").tell map (_ => f(a))
}
val d2s: KWriter[Double, String] = lift(s => s"$s%")
val s2n: KWriter[String, Int]    = lift(_.length)
val n2d: KWriter[Int, Double]    = lift(_ / 100.0)

def id[A] = implicitly[Category[KWriter]].id[A]

// 結合法則
((n2d >>> d2s) >>> s2n) run 1234
(n2d >>> (d2s >>> s2n)) run 1234

// 単位法則
(id >>> n2d) run 1234
n2d run 1234
(n2d >>> id) run 1234
