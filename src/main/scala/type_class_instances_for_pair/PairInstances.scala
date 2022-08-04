package type_class_instances_for_pair

import cats.kernel.CommutativeMonoid
import cats.*
import cats.syntax.monoid.*
import cats.syntax.semigroupal.*
import cats.syntax.functor.*

object instances:
  type Pair[A] = (A, A)

  given [A]: Eq[Pair[A]] = Eq.fromUniversalEquals

  // ---------------------------------------------------------------------------------
  // Invariant - Functor - Contravariant

  trait PairInvariant extends Invariant[Pair]:
    override def imap[A, B](fa: (A, A))(f: A => B)(g: B => A): (B, B) = fa match
      case (a1, a2) => (f(a1), f(a2))

  trait PairFunctor extends Functor[Pair] with PairInvariant:
    override def map[A, B](fa: (A, A))(f: A => B): (B, B) = fa match
      case (a1, a2) => (f(a1), f(a2))

  // Contravariant[Pair]
  // contramap: (B -> A) -> (A, A) -> (B, B) が無理なので定義できない

  // ---------------------------------------------------------------------------------

  trait PairSemigroupal extends Semigroupal[Pair]:
    def product[A, B](fa: (A, A), fb: (B, B)): Pair[(A, B)] = (fa, fb) match
      case ((a1, a2), (b1, b2)) => ((a1, b1), (a2, b2))

  trait PairInvariantSemigroupal extends InvariantSemigroupal[Pair] with PairSemigroupal with PairInvariant

  trait PairInvariantMonoidal extends InvariantMonoidal[Pair] with PairInvariantSemigroupal:
    def unit: (Unit, Unit) = ((), ())

  // ContravariantSemigroupal[Pair]
  // ContravariantMonoidal[Pair]

  // ---------------------------------------------------------------------------------
  trait PairApply extends Apply[Pair] with PairFunctor:
    override def ap[A, B](ff: (A => B, A => B))(fa: (A, A)): (B, B) = (ff, fa) match
      case ((f1, f2), (a1, a2)) => (f1(a1), f2(a2))

  trait PairCommutativeApply extends CommutativeApply[Pair] with PairApply

  trait PairApplicative extends Applicative[Pair] with PairApply:
    def pure[A](a: A): (A, A) = (a, a)

  trait PairCommutativeApplicative extends CommutativeApplicative[Pair] with PairApplicative

  trait PairFlatMap extends FlatMap[Pair] with PairApply:
    def flatMap[A, B](fa: (A, A))(f: A => (B, B)): (B, B) = map(fa)(f) match
      case ((b1, _), (_, b2)) => (b1, b2)

    def tailRecM[A, B](a: A)(f: A => (Either[A, B], Either[A, B])): (B, B) =
      @scala.annotation.tailrec
      def first(a: A): B = f(a) match
        case (Right(b), _) => b
        case (Left(a) , _) => first(a)

      @scala.annotation.tailrec
      def second(a: A): B = f(a) match
        case (_, Right(b)) => b
        case (_, Left(a) ) => second(a)

      (first(a), second(a))

    // stack unsafe implementation
//    override def tailRecM[A, B](a: A)(f: A => (Either[A, B], Either[A, B])): (B, B) = f(a) match {
//      case (Right(b1), Right(b2)) => (b1, b2)
//      case (Left(a1),  Right(b2)) => (tailRecM(a1)(f)._1, b2)
//      case (Right(b1), Left(a2))  => (b1, tailRecM(a2)(f)._2)
//      case (Left(a1),  Left(a2))  => (tailRecM(a1)(f)._1, tailRecM(a2)(f)._2)
//    }

  trait PairCommutativeFlatMap extends CommutativeFlatMap[Pair] with PairFlatMap with PairCommutativeApply

  trait PairMonad extends Monad[Pair] with PairFlatMap with PairApplicative

  trait PairCommutativeMonad extends CommutativeMonad[Pair]
    with PairMonad
    with PairCommutativeFlatMap
    with PairCommutativeApplicative


  trait PairCommutativeMonad2 extends CommutativeMonad[Pair]:
    override def pure[A](a: A): (A, A) = (a, a)
    override def map[A, B](fa: (A, A))(f: A => B): (B, B) = fa match
      case (a1, a2) => (f(a1), f(a2))

    override def flatMap[A, B](fa: (A, A))(f: A => (B, B)): (B, B) = map(fa)(f) match
      case ((b1, _), (_, b2)) => (b1, b2)

    override def tailRecM[A, B](a: A)(f: A => (Either[A, B], Either[A, B])): (B, B) =
      @scala.annotation.tailrec
      def first(a: A): B = f(a) match
        case (Right(b), _) => b
        case (Left(a) , _) => first(a)

      @scala.annotation.tailrec
      def second(a: A): B = f(a) match
        case (_, Right(b)) => b
        case (_, Left(a) ) => second(a)

      (first(a), second(a))

  // ---------------------------------------------------------------------------------
  trait PairCoflatMap extends CoflatMap[Pair] with PairFunctor:
    override def coflatMap[A, B](fa: (A, A))(f: ((A, A)) => B): (B, B) =
      val x = f(fa)
      (x, x)

  // unlawful
  trait PairComonad extends Comonad[Pair] with PairCoflatMap:
    def extract[A](x: (A, A)): A = x._2

  trait PairDistributive extends Distributive[Pair] with PairFunctor:
    override def distribute[G[_], A, B](ga: G[A])(f: A => (B, B))(using Functor[G]): (G[B], G[B]) =
      val gpb: G[(B, B)] = ga map f
      (gpb.map(_._1), gpb.map(_._2))

  // ---------------------------------------------------------------------------------
  // ---------------------------------------------------------------------------------
  trait PairUnorderedFoldable extends UnorderedFoldable[Pair]:
    override def unorderedFoldMap[A, B: CommutativeMonoid](fa: (A, A))(f: A => B): B = fa match
      case (a1, a2) => f(a1) |+| f(a2)

  trait PairUnorderedTraverse extends UnorderedTraverse[Pair] with PairUnorderedFoldable:
    override def unorderedTraverse[G[_]: CommutativeApplicative, A, B](sa: (A, A))(f: A => G[B]): G[(B, B)] =
      sa match
        case (a1, a2) => f(a1) product f(a2)

  trait PairFoldable extends Foldable[Pair]:
    def foldLeft[A, B](fa: (A, A), b: B)(f: (B, A) => B): B = fa match
      case (a1, a2) => f(f(b, a1), a2)

    def foldRight[A, B](fa: (A, A), lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa match
        case (a1, a2) => Eval.defer(f(a1, Eval.defer(f(a2, lb))))

  trait PairReducible extends Reducible[Pair] with PairFoldable:
    def reduceLeftTo[A, B](fa: (A, A))(f: A => B)(g: (B, A) => B): B = fa match
      case (a1, a2) => g(f(a1), a2)

    def reduceRightTo[A, B](fa: (A, A))(f: A => B)(g: (A, Eval[B]) => Eval[B]): Eval[B] = fa match
      case (a1, a2) => g(a1, Eval.now(f(a2)))

  trait PairTraverse extends Traverse[Pair] with PairFunctor with PairFoldable:
    override def traverse[G[_]: Applicative, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] =
      fa match
        case (a1, a2) => f(a1) product f(a2)

  trait PairNonEmptyTraverse extends NonEmptyTraverse[Pair] with PairReducible:
    def nonEmptyTraverse[G[_]: Apply, A, B](fa: (A, A))(f: A => G[B]): G[(B, B)] = fa match
      case (a1, a2) => f(a1) product f(a2)

//  def pairContraVariant: Contravariant[Pair] = new Contravariant[Pair] {
//    override def contramap[A, B](fa: (A, A))(f: B => A): (B, B) = ???
//  }
//  def pairInvariantSemigroupal: InvariantSemigroupal[Pair] =
//    new InvariantSemigroupal[Pair] with PairInvariant with PairSemigroupal {}

  trait PairSemigroupK extends SemigroupK[Pair]:
    override def combineK[A](x: (A, A), y: (A, A)): (A, A) = (x, y) match 
      case ((x1, _), (_, y2)) => (x1, y2)

