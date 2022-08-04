package type_class_instances_for_pair

import cats._
import cats.data.Const
import cats.laws.discipline.*
import cats.instances.int.*
import cats.instances.double.*
import cats.instances.string.*
import org.scalacheck.{Arbitrary, Gen}
import munit.DisciplineSuite
import type_class_instances_for_pair.instances.*

// -----------------------------------------------------------------------------------------------------------------
class PairInvariantTests extends DisciplineSuite:
  given Invariant[Pair] = new PairInvariant {}
  checkAll("Pair.InvariantLaws", InvariantTests[Pair].invariant[Int, Int, String])

class PairFunctorTests extends DisciplineSuite:
  given Functor[Pair] = new PairFunctor {}
  checkAll("Pair.FunctorLaws", FunctorTests[Pair].functor[Int, Int, String])

// -----------------------------------------------------------------------------------------------------------------
class PairSemigroupalTests extends DisciplineSuite:
  given Invariant[Pair]   = new PairInvariant {}
  given Semigroupal[Pair] = new PairSemigroupal {}
  checkAll("Pair.SemigroupalLaws", SemigroupalTests[Pair].semigroupal[Int, Int, String])

class PairInvariantSemigroupalTests extends DisciplineSuite:
  given InvariantSemigroupal[Pair] = new PairInvariantSemigroupal {}
  checkAll("Pair.InvariantSemigroupalLaws", InvariantSemigroupalTests[Pair].invariantSemigroupal[Int, Int, String])

class PairInvariantMonoidalTests extends DisciplineSuite:
  given InvariantMonoidal[Pair] = new PairInvariantMonoidal {}
  checkAll("Pair.InvariantMonoialLaws", InvariantMonoidalTests[Pair].invariantMonoidal[Int, Int, String])

// -----------------------------------------------------------------------------------------------------------------
class PairUnorderedFoldableTests extends DisciplineSuite:
  given UnorderedFoldable[Pair] = new PairUnorderedFoldable {}
  checkAll("Pair.UnorderedFoldableLaws", UnorderedFoldableTests[Pair].unorderedFoldable[Int, Double])

class PairUnorderedTraverseTests extends DisciplineSuite:
  given UnorderedTraverse[Pair] = new PairUnorderedTraverse {}

  type IntConst[T] = Const[Int, T]
  given Arbitrary[IntConst[Int]] = Arbitrary[IntConst[Int]] {
    Gen.choose(-3, 3).map(Const[Int, Int])
  }
  checkAll(
    "Pair.UnorderedTraverseLaws",
    UnorderedTraverseTests[Pair].unorderedTraverse[Int, Int, Int, IntConst, IntConst])

// -----------------------------------------------------------------------------------------------------------------
class PairApplyTests extends DisciplineSuite:
  given Apply[Pair] = new PairApply {}
  checkAll("Pair.ApplyLaws", ApplyTests[Pair].apply[Int, Int, String])

class PairCommutativeApplyTests extends DisciplineSuite:
  given CommutativeApply[Pair] = new PairCommutativeApply {}
  checkAll("Pair.CommutativeApplyLaws", CommutativeApplyTests[Pair].commutativeApply[Int, Int, String])

class PairFlatMapTests extends DisciplineSuite:
  given FlatMap[Pair] = new PairFlatMap {}
  checkAll("Pair.FlatMapLaws", FlatMapTests[Pair].flatMap[Int, Int, String])

class PairCommutativeFlatMapTests extends DisciplineSuite:
  given CommutativeFlatMap[Pair] = new PairCommutativeFlatMap {}
  checkAll("Pair.CommutativeFlatMapLaws", CommutativeFlatMapTests[Pair].commutativeFlatMap[Int, Int, String])

class PairApplicativeTests extends DisciplineSuite:
  given Applicative[Pair] = new PairApplicative {}
  checkAll("Pair.ApplicativeLaws", ApplicativeTests[Pair].applicative[Int, Int, String])

class PairCommutativeApplicativeTests extends DisciplineSuite:
  given CommutativeApplicative[Pair] = new PairCommutativeApplicative {}
  checkAll("Pair.CommutativeApplicativeLaws", CommutativeApplicativeTests[Pair].commutativeApplicative[Int, Int, String])

class PairMonadTests extends DisciplineSuite:
  given Monad[Pair] = new PairMonad {}
  checkAll("Pair.MonadLaws", MonadTests[Pair].monad[Int, Int, String])

class PairCommutativeMonadTests extends DisciplineSuite:
  given CommutativeMonad[Pair] = new PairCommutativeMonad {}
  checkAll("Pair.CommutativeMonadLaws", CommutativeMonadTests[Pair].commutativeMonad[Int, Int, String])

class PairCommutativeMonad2Tests extends DisciplineSuite:
  given CommutativeMonad[Pair] = new PairCommutativeMonad2 {}
  checkAll("Pair.CommutativeMonadLaws", CommutativeMonadTests[Pair].commutativeMonad[Int, Int, String])

class PairCoflatMapTests extends DisciplineSuite:
  given CoflatMap[Pair] = new PairCoflatMap {}
  checkAll("Pair.CoflatMapLaws", CoflatMapTests[Pair].coflatMap[Int, Int, Int])

//class PairComonadTests extends DisciplineSuite:
//  implicit val i1: Comonad[Pair] = new PairComonad {}
//  checkAll("Pair.ComonadLaws", ComonadTests[Pair].comonad[Int, Int, Int])
//}
class PairDistributiveTests extends DisciplineSuite:
  import cats.instances.option._
  given Distributive[Pair] = new PairDistributive {}
  checkAll(
    "Pair.DistributiveLaws",
    DistributiveTests[Pair].distributive[Int, Int, Int, Option, Pair])


class PairFoldableTests extends DisciplineSuite:
  import cats.instances.option._
  given Foldable[Pair] = new PairFoldable {}
  checkAll("Pair.FoldableLaws", FoldableTests[Pair].foldable[Int, Int])

class PairReducibleTests extends DisciplineSuite:
  import cats.instances.option._
  import cats.instances.unit._
  given Reducible[Pair] = new PairReducible {}
  checkAll("Pair.ReducibleLaws", ReducibleTests[Pair].reducible[Option, Int, Int])

class PairTraverseTests extends DisciplineSuite:
  import cats.instances.option._
  type IntConst[T] = Const[Int, T]
  given Arbitrary[IntConst[Int]] = Arbitrary[IntConst[Int]] {
    Gen.choose(-3, 3).map(Const[Int, Int])
  }
  given Traverse[Pair] = new PairTraverse {}
  checkAll("Pair.TraverseLaws", TraverseTests[Pair].traverse[Int, Int, Int, Int, IntConst, IntConst])

class PairNonEmptyTraverseTests extends DisciplineSuite:
  import cats.instances.option._
  import cats.instances.unit._
  type IntConst[T] = Const[Int, T]
  given Arbitrary[IntConst[Int]] = Arbitrary[IntConst[Int]] {
    Gen.choose(-3, 3).map(Const[Int, Int])
  }
  given NonEmptyTraverse[Pair] = new PairNonEmptyTraverse {}
  checkAll(
    "Pair.NonEmptyTraverseLaws",
    NonEmptyTraverseTests[Pair].nonEmptyTraverse[Option, Int, Int, Int, Int, IntConst, IntConst])

class PairSemigroupKTests extends DisciplineSuite:
  given SemigroupK[Pair] = new PairSemigroupK {}
  checkAll("Pair.SemigroupKLaws", SemigroupKTests[Pair].semigroupK[Int])


