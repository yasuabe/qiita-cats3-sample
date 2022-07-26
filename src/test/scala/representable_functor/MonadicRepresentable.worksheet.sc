import cats.{Functor, Monoid, Representable}
import cats.instances.stream._
import cats.syntax.representable._
import representable_functor.Triple
import representable_functor.Triple.given

type Sign = "minus" | "zero" | "plus"

val triple: Triple[Sign] = ("minus", "zero", "plus")
(-10 to 10 by 5) map triple.index

val g = (n: Int) => if (n < 0) '-' else if (n > 0) '+' else '0'
g.tabulate[Triple]
g.tabulate[Triple].index.apply(10)

val monad = Representable.monad[Triple]
monad.pure('x')
monad.flatMap(('x', 'y', 'z'))(a => (s"1$a", s"2$a", s"3$a"))

val monAdd = new Monoid[Int]:
  def empty: Int = 0
  def combine(x: Int, y: Int): Int = x + y

val monMul = new Monoid[Int]:
  def empty: Int = 1
  def combine(x: Int, y: Int): Int = x * y

val repr = summon[Representable[Triple]]

val bmAdd = Representable.bimonad[Triple, Int](repr, monAdd)
val bmMul = Representable.bimonad[Triple, Int](repr, monMul)

bmAdd.extract(('x', 'y', 'z'))
bmMul.extract(('x', 'y', 'z'))

bmAdd.coflatMap(('x', 'y', 'z'))(t => s"$t")
bmMul.coflatMap(('x', 'y', 'z'))(t => s"$t")
