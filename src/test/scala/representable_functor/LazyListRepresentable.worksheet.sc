import representable_functor.given
import cats.syntax.representable.*

val index = LazyList.continually("abc".to(LazyList)).flatten.index
(5 until 10) map (n => s"$n:${ index(n) }") // Vector(5:c, 6:a, 7:b, 8:c, 9:a)

val tabulate = ((n: Int) => ('x' + (n % 3)).toChar).tabulate[LazyList]
tabulate.take(5).toList                   // List(x, y, z, x, y)
