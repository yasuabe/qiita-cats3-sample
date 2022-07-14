package partially_applied_enhanced

import cats.Applicative
import cats.data.OptionT
import cats.syntax.option.*
import cats.syntax.applicative.*

// cats guidline の サンプル
final class PurePartiallyApplied[F[_]](val dummy: Boolean = true ) extends AnyVal {
  def apply[A](value: A)(implicit F: Applicative[F]): OptionT[F, A] =
    OptionT(F.pure(Some(value)))
}
def pure2[F[_]]: PurePartiallyApplied[F] = new PurePartiallyApplied[F]

// Scala 3 と Cats でまずスッキリさせる. AnyVal で最適化している部分は保留
final class PurePartiallyApplied3[F[_]]:
  def apply[A](value: A)(using Applicative[F]): OptionT[F, A] =
    OptionT(value.some.pure)

def pure3[F[_]]: PurePartiallyApplied3[F] = PurePartiallyApplied3[F]

// Context Function で using を出す
final class PurePartiallyApplied4[F[_]]:
  def apply[A](value: A): Applicative[F] ?=> OptionT[F, A] =
    OptionT(value.some.pure)

def pure4[F[_]]: PurePartiallyApplied4[F] = PurePartiallyApplied4[F]

// 関数にする
final class PurePartiallyApplied5[F[_]]:
  val pure: Applicative[F] ?=> [A] => A => OptionT[F, A] =
    [A] => (a: A) => OptionT(a.some.pure)

def pure5[F[_]: Applicative] = PurePartiallyApplied5[F].pure

// クラスが要らなくなった
def pure[F[_]]: Applicative[F] ?=> [A] => A => OptionT[F, A] =
  [A] => (a: A) => OptionT(a.some.pure)
