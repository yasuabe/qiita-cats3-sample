package interesting_type_classes

import cats.kernel.laws.discipline.GroupTests
import cats.{ Eq, Group }
import org.scalacheck.{ Arbitrary, Gen }
import munit.DisciplineSuite
import Amida.*

opaque type Amida = List[Int]

object Amida:
  val Lines = 4
  val Empty: Amida = List.empty[Int]
  def apply(legs: List[Int]): Amida = legs

  extension (self: Amida)
    def combine(another: Amida): Amida = self ++ another
    def inverse                : Amida = self.reverse
    def permutation: Vector[Int] = self.foldLeft((0 until Lines).toVector)(
     (acc, l) => acc.updated(l, acc(l + 1)).updated(l + 1, acc(l)))

given Group[Amida] with
  def inverse(a: Amida): Amida           = a.inverse
  def empty: Amida                       = Amida.Empty
  def combine(x: Amida, y: Amida): Amida = x combine y

class GroupSpec extends DisciplineSuite:
  import Gen.{chooseNum, listOfN}

  given Eq[Amida] = _.permutation == _.permutation
  given Arbitrary[Amida] = Arbitrary {
    for {
      len  <- chooseNum(0, 5)
      legs <- listOfN(len, chooseNum(0, Lines - 2))
    } yield Amida(legs)
  }
  checkAll("group of Amida", GroupTests[Amida].group)
