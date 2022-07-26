package representable_functor

import cats.instances.AllInstances
import cats.laws.discipline.RepresentableTests
import cats.{ Eq, Functor, Representable }
import org.scalacheck.{ Arbitrary, Gen }
import cats.instances.stream.*
import munit.DisciplineSuite

given Representable.Aux[LazyList, Int] = new Representable[LazyList]:
  type Representation      = Int
  def F: Functor[LazyList] = summon

  def tabulate[A](f: Int => A): LazyList[A] = LazyList.from(0) map f
  def index[A](f: LazyList[A]): Int => A    = f.apply

class LazyListSuite extends DisciplineSuite:
  val genNonNegative : Gen[Int]            = Gen.chooseNum(0, 4)
  val genChar        : Gen[Char]           = Gen.oneOf('a', 'b', 'c')
  val genCharLazyList: Gen[LazyList[Char]] = Gen.listOfN(5, genChar).map(_.to(LazyList))

  given Arbitrary[Int]            = Arbitrary(genNonNegative)
  given Arbitrary[LazyList[Char]] = Arbitrary(genCharLazyList)
  given Eq[LazyList[Char]]        = _.take(5) == _.take(5)

  checkAll("Representablity of LazyList", RepresentableTests[LazyList, Int].representable[Char])