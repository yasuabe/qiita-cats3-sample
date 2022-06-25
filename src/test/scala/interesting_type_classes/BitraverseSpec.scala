package interesting_type_classes

import cats.{ Apply, Eq, Group }
import cats.data.Chain
import cats.syntax.apply.*
import org.scalacheck.cats.implicits.*
import cats.laws.discipline.BitraverseTests
import org.scalacheck.{ Arbitrary, Gen }
import munit.DisciplineSuite

type TupleChain[A, B] = Chain[(A, B)]

class BitraverseSpec extends DisciplineSuite:
  import Gen.{ chooseNum, listOfN }

  given Arbitrary[TupleChain[Int, Int]] = Arbitrary(
    for {
      len    <- chooseNum(0, 3)
      tuples <- listOfN(len, (chooseNum(0, 3), chooseNum(0, 3)).mapN((_, _)))
    } yield Chain.fromSeq(tuples)
  )
  checkAll("bt", BitraverseTests[TupleChain].bitraverse[Option, Int, Int, Int, Int, Int, Int])
