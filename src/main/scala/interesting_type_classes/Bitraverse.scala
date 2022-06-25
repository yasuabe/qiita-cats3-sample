package interesting_type_classes

import cats.data.Chain
import cats.instances.option.*
import cats.instances.tuple.*
import cats.syntax.bifoldable.*
import cats.syntax.bitraverse.*
import cats.syntax.traverse.*
import cats.{Applicative, Bitraverse, Eval}

type TupleChain[A, B] = Chain[(A, B)]

given Bitraverse[TupleChain] with
  def bitraverse[G[_]: Applicative, A, B, C, D](
    fab: TupleChain[A, B])(f: A => G[C], g: B => G[D]): G[TupleChain[C, D]] =
    fab.map(_.bitraverse(f, g)).sequence

  def bifoldLeft[A, B, C](fab: TupleChain[A, B], c: C)(f: (C, A) => C, g: (C, B) => C): C =
    fab.foldLeft(c)((cc, d) => d.bifoldLeft(cc)(f, g))

  def bifoldRight[A, B, C](fab: TupleChain[A, B], c: Eval[C])(
    f: (A, Eval[C]) => Eval[C],
    g: (B, Eval[C]) => Eval[C]
  ) = fab.foldRight(c)(_.bifoldRight(_)(f, g))
