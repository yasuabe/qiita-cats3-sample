import interesting_type_classes.{ given, _ }
import cats.data.Chain
import cats.Eval
import cats.syntax.option._
import cats.syntax.bifoldable._
import cats.syntax.bitraverse._

val tc1: TupleChain[Char, Double] = Chain(('a', 0.1), ('b', 0.2), ('c', 0.3))
val tc2: TupleChain[Char, Double] = Chain(('a', 1.5), ('b', -2.5))

val maybePercent = (d: Double) => Option.when(d >= 0)(s"${d * 100}%")
tc1.bitraverse(_.toUpper.some, maybePercent)
// Option[TupleChain[Char,String]] = Some(Chain((A,10.0%), (B,20.0%), (C,30.0%)))

tc2.bitraverse(_.toUpper.some, maybePercent)
// Option[TupleChain[Char,String]] = None

tc1.bifoldLeft("")((s, n) => s + s"<$n", (s, n) => s + s",$n>, ")
// String = "<a,0.1>, <b,0.2>, <c,0.3>, "

tc1.bifoldRight(Eval.now(100.0))((a, c) => c.map(_ - (a-'a')), (b, c) => c.map(_ + b)).value
//  Double = 97.6
