import cats.syntax.representable.*
import representable_functor.Triple
import representable_functor.given

type Sign = "minus" | "zero" | "plus"

val triple: Triple[Sign] = ("minus", "zero", "plus")
(-10 to 10 by 5) map triple.index
// res2 = Vector(minus, minus, zero, plus, plus)

val g = (n: Int) => if n < 0 then '-' else if n > 0 then '+' else '0'
g.tabulate[Triple]
// res3: Triple[Char] = (-,0,+)

g.tabulate[Triple].index.apply(10)
// res4: Char = +