import cats.mtl.Raise
import cats.syntax.applicative.*
import cats.{ Applicative, Semigroupal }
import cats.data.Validated

def parseNumber[F[_]: Applicative](in: String)(using F: Raise[F, String]): F[Double] =
  if in.matches("-?[0-9]+") then in.toDouble.pure[F]
                            else F.raise(in) // raise 構文

val result: Validated[String, (Double, Double, Double)] = Semigroupal.tuple3(
  parseNumber[Validated[String, *]]("abc"),
  parseNumber[Validated[String, *]]("123"),
  parseNumber[Validated[String, *]]("xyz")
)
// result = Invalid(abcxyz)