import cats.arrow.Arrow
import cats.data.{Cokleisli, NonEmptyList}
import cats.instances.function.*
import cats.syntax.compose.*
import cats.syntax.strong.*
import cats.syntax.arrow.*

val A = Arrow[Function]
given Arrow[Function] = A

// lift 
val f1 = Arrow[Function].lift(1d / (_: Int))
f1(10)

// compose 構文 >>> 
val f2: Int => String = (1d / (_: Int)) >>> (s => s"$s%")
f2(10)

// first
val f3: ((Int, String)) => (Double, String) = (1d / (_: Int)).first[String]
f3((10, "ten"))

// second 
val f4: ((Int, Double)) => (Int, String) = ((_: Double).toString.reverse).second[Int]
f4((10, 3.14))

// first >>> second
val f5: ((Int, Double)) => (Double, String) =
(1d / (_: Int)).first >>> ((_: Double).toString.reverse).second
f5((10, 3.14))

// cokleisli
def f6(using A: Arrow[[A, B] =>> Cokleisli[NonEmptyList, A, B]]) = A.lift[String, Int](_.length)
f6.run(NonEmptyList.of("abc", "ab", "c"))
