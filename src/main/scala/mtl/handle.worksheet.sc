import cats.Applicative
import cats.data.Validated
import cats.syntax.traverse.*
import cats.syntax.applicative.*
import cats.mtl.Raise
import cats.mtl.syntax.handle.*

def parseNumber[F[_]: Applicative](in: String)(using F: Raise[F, String]): F[Double] =
  if in.matches("-?[0-9]+") then in.toDouble.pure[F]
                            else F.raise(in) // raise 構文

List("100", "abc", "200", "def") traverse parseNumber[Validated[String, *]]
// Invalid(abcdef)

List("100", "abc", "200", "def") traverse { s =>
  parseNumber[Validated[String, *]](s).handle[String](_ => Double.NaN) // handle 構文。
}
// Valid(List(100.0, NaN, 200.0, NaN))
