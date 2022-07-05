package interesting_type_classes

import scala.math.abs
import cats.Eq
import munit.DisciplineSuite
import org.scalacheck.{ Arbitrary, Gen }
import cats.kernel.CommutativeMonoid
import cats.kernel.laws.discipline.CommutativeMonoidTests

opaque type AbsEven = Int

object AbsEven:
  def apply(n: Int): AbsEven = abs(n / 2 * 2)
  extension (self: AbsEven) infix def + (another: AbsEven) = self + another

given CommutativeMonoid[AbsEven] with
  def empty = AbsEven(0)
  def combine(x: AbsEven, y: AbsEven): AbsEven = x + y

class AbsEvenCommutativeMonoidSpec extends DisciplineSuite:
  import Gen.chooseNum

  given Eq[AbsEven]        = _ == _
  given Arbitrary[AbsEven] = Arbitrary(chooseNum(-10, 10).map(AbsEven(_)))

  checkAll("AbsEven", CommutativeMonoidTests[AbsEven].commutativeMonoid)
