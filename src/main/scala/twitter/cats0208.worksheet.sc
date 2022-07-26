import cats.Endo
import cats.data.State
import cats.syntax.apply.*
import cats.syntax.applicative.*

// List
(List(1, 2), List('a', 'b')).flatMapN(List(_, _))

// Function
case class Env(name: String, count: Int, format: Endo[String])
val f4 = ((_: Env).name, (_: Env).count).flatMapN((s, n) => (_: Env).format(s * n))
f4(Env("foo", 3, s => s"<$s>"))

// State
val inc = State.modify[Int](1 + _).get
val prog = (inc, inc, inc).flatMapN((_, _, _).pure[State[Int, *]])
prog.runA(10).value
