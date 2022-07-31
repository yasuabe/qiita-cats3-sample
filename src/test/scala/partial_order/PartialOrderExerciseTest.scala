package partial_order

import java.time.LocalDate
import java.time.temporal.ChronoUnit.{DAYS, MONTHS}

import cats.syntax.foldable.*
import cats.{Foldable, Monad, Monoid, PartialOrder}
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import cats.kernel.laws.discipline.PartialOrderTests
import org.scalacheck.{Arbitrary, Gen}
import munit.DisciplineSuite


class PartialOrderExerciseTest extends DisciplineSuite:
  // 約数 -------------
  type Nat = Int Refined Positive

  given PartialOrder[Nat] = (a: Nat, b: Nat) =>
    val compare = (_: Nat).value % (_: Nat).value
    (compare(a, b), compare(b, a)) match
      case (0, 0) =>  0.0
      case (0, _) => -1.0
      case (_, 0) =>  1.0
      case _      => Double.NaN

  // // 期間 -------------
  case class FromTo(from: LocalDate, to: LocalDate)

  given PartialOrder[FromTo] = (a: FromTo, b: FromTo) =>
    val compare = (inner: FromTo, outer: FromTo) =>
      !(inner.from isBefore outer.from) && !(outer.to isAfter inner.to)

    (compare(a, b), compare(b, a)) match
      case (true, true) =>  0.0
      case (true, _   ) => -1.0
      case (_   , true) =>  1.0
      case _            => Double.NaN

  // // 権限 -------------
  object Permission:
    opaque type Permission = Int

    val Empty: Permission = 0
    val Exec : Permission = 1
    val Write: Permission = 2
    val Read : Permission = 4

    def apply(n: Int): Permission = n
    extension (x: Permission) def +(y: Permission): Permission = x | y

    given Monoid[Permission] with
      val empty: Permission = Permission.Empty
      def combine(x: Permission, y: Permission): Permission = x + y

    given PartialOrder[Permission] = (a: Permission, b: Permission) =>
      def compare(a: Permission, b: Permission) = (a | b) == b
      (compare(a, b), compare(b, a)) match
        case (true, true) =>  0.0
        case (true, _   ) => -1.0
        case (_   , true) =>  1.0
        case _            => Double.NaN

  // 共通 -------------
  private val smallIntGen = Gen.chooseNum[Int](1, 100)

  // 約数 -------------
  {
    given Arbitrary[Nat] = Arbitrary(
      smallIntGen.map(refineV[Positive](_).toOption.get))

    given Arbitrary[Nat => Nat] =
      Arbitrary(smallIntGen.map(n => nat => refineV[Positive](nat.value + n).toOption.get))

    checkAll("nat divisibility", PartialOrderTests[Nat].partialOrder)
  }

  // 期間 -------------
  {
    val d0 = LocalDate.of(2018, 1, 1)

    given Arbitrary[FromTo] = Arbitrary(
      for
        offset <- Gen.chooseNum(0, 11)
        period <- Gen.chooseNum(1, 12 - offset)
      yield FromTo(d0.plus(offset, MONTHS), d0.plus(offset + period, MONTHS).minus(1, DAYS)))

    given Arbitrary[FromTo => FromTo] =
      Arbitrary(smallIntGen.map(n => ft => FromTo(ft.from.plus(n, DAYS), ft.to.plus(n, DAYS))))

    checkAll("period inclusion", PartialOrderTests[FromTo].partialOrder)
  }

  // 権限 -------------
  {
    import Permission.*

    given Monad[Gen] with
      def pure[A](a: A): Gen[A] = Gen.const(a)
      def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] = fa flatMap f
      def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] =
        f(a).flatMap(_.fold(tailRecM(_)(f), pure))

    val ifTrue = (p: Permission) => Gen.oneOf(true, false).map(if _ then Empty else p)

    given Arbitrary[Permission] =
      Arbitrary(List(Exec, Read, Write).foldMapM(ifTrue))

    given Arbitrary[Permission => Permission] =
      Arbitrary(Gen.chooseNum[Int](0, 7).map(n => p => Permission(n) + p))

    checkAll("permission inclusion", PartialOrderTests[Permission].partialOrder)
  }
