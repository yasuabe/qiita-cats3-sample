import scala.util.Try
import cats.syntax.option.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*

enum Op(s: String, val f: (Double, Double) => Option[Double]):
  case Plus  extends Op("+", (m, n) => (m + n).some)
  case Minus extends Op("-", (m, n) => (m - n).some)
  case Mult  extends Op("*", (m, n) => (m * n).some)
  case Div   extends Op("/", (m, n) => Try(m / n).toOption)
  override def toString(): String = s

val ops = for {
  o <- Op.values
  p <- Op.values
  q <- Op.values
} yield (o, p, q)

type Calc = (Int, Op, Int, Op, Int, Op, Int) => Option[Double]
type Func = (Int, Op, Int, Op, Int, Op, Int) => Option[String]
val f1: Calc = (n1: Int, o1: Op, n2: Int, o2: Op, n3: Int, o3: Op, n4: Int) => ???
// val f2: Calc = (n1, o1, n2, o2, n3, o3, n4) => o3.f(o2.f(o1.f(n1, n2), n3), n4)

enum X(s: String, f: Calc):
  case E1 extends X("(((%d %s %d) %s %d) %s %d)", (a, o, b, p, c, q, d) => o.f(a, b).flatMap(p.f(_, c)).flatMap(q.f(_, d)))
  case E2 extends X("((%d %s (%d %s %d)) %s %d)", (a, o, b, p, c, q, d) => o.f(b, c).flatMap(p.f(a, _)).flatMap(q.f(_, d)))
  // case E3 extends X("((%d %s %d) %s (%d %s %d))", (a, o, b, p, c, q, d) => q.f(o.f(a, b), p.f(c, d)))
  // case E4 extends X("(%d %s ((%d %s %d) %s %d))", (a, o, b, p, c, q, d) => q.f(a, p.f(o.f(b, c), d)))
  // case E5 extends X("(%d %s (%d %s (%d %s %d)))", (a, o, b, p, c, q, d) => q.f(a, p.f(b, o.f(c, d))))
  def judge: Func = (a, o, b, p, c, q, d) =>
    f(a, o, b, p, c, q, d).filter(_ == 48).as(s.format(a, o, b, p, c, q, d))

val y = for {
  ns <- List(3, 6, 6, 8).permutations.distinct
  os <- ops
  x <- X.values
} yield {
  val a :: b :: c :: d :: Nil = ns
  val o :: p :: q :: Nil  = os.toList
  x.judge(a, o, b, p, c, q, d)
}
y.flatten.toList
1

