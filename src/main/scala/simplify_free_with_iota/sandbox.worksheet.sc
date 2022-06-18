import cats.Id
import cats.data.EitherK
import cats.~>
import cats.arrow.FunctionK

enum CopK[F[_]]:
  case HCons[F[_], G[_]](g: CopK[G]) extends CopK[F]
  // case infix [F[_], G[_]] (EitherK[]) :~: (g: CopK[G]) extends CopK[F]
  case Hnil extends CopK[Nothing]

infix type :++: [F[_], G[_]] = [X] =>> EitherK[F, G, X]
infix type :+: [F[_], G[_]] = EitherK[F, G, ?]

// enum AlgeA[-T]:
//   case A1[X]() extends AlgeA[X]
// enum AlgeB[-T]:
//   case B1[X]() extends AlgeB[X]
// enum AlgeC[-T]:
//   case C1[X]() extends AlgeC[X]


// type Rotate[X[_]] = X[_] match
//   case AlgeA[?] => AlgeB
//   case AlgeB[?] => AlgeC
//   case AlgeC[?] => AlgeA

// type Rotated = [T] =>> Rotate[AlgeA][T]
// val algeC: Rotated[String] = AlgeB.B1()


// val abc: ABC[String] = EitherK.leftc(AlgeA.A1[String]())
// infix type :^:  kkk



enum AlgeA[-T]:
  case A1[X](v: X) extends AlgeA[X]
enum AlgeB[-T]:
  case B1[X](v: X) extends AlgeB[X]

type Invert[F[_]] = F[_] match
  case AlgeA[?] => AlgeB
  case AlgeB[?] => AlgeA

val a0: AlgeA[String] = AlgeA.A1("a0")
val b1: Invert[AlgeA][String] = AlgeB.B1("b1")
val a1: Invert[Invert[AlgeA]][String] = AlgeA.A1("a1")

import cats.data.Kleisli
import cats.data.OptionT
import cats.effect.IO
import cats.syntax.option.*

type Extract1stO[F] = F match
  case Kleisli[e, _, _] => e 
  case EitherK[_, g, _] => g 
  case _                => Option

type O1[A] = Extract1stO[Kleisli[IO, Int, A]][A]
type O2[A] = Extract1stO[EitherK[Option, List, Any]][A]
type O3[A] = Extract1stO[AnyVal][A]
val o1: O1[Int] = IO.pure(1)
val o2: O2[Int] = List(2)
val o3: O3[Int] = Option(3)


type Extract[F[_]] = F[_] match
  case OptionT[f, _] => f 
  case _             => Id

type T1 = [X] =>> OptionT[List, X]

// val v1: Extract[T1][Int] = List(1) // こっち期待
val v1: Extract[T1][Int] = Id(1)

class Nat(val x: Int):
  def get: Int = x
  def isEmpty = x < 0

object Nat:
  def unapply(x: Int): Nat = Nat(x)

5 match
  case Nat(n) => println(s"$n is a natural number")
  case _      => ()
1 + 1


