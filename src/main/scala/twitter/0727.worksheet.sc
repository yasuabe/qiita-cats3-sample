import cats.Monoid
import cats.data.NonEmptyList
import cats.syntax.foldable.*

(BigInt(1) to BigInt(100)).reduce((n, m) => n * m / (n gcd m))
1