package profunctor_sample

import scala.compiletime.{error, requireConst}
import cats.syntax.either.*
import scala.quoted.*

object Name:
  opaque type Name <: String = String

  def apply(s: String): Either[String, Name] =
    if s.matches("[a-z]+") then s.asRight else "error".asLeft

  inline def from(inline s: String): Name = ${ fromImpl('s) }

  private def fromImpl(s: Expr[String])(using Quotes): Expr[Name] =
    import quotes.reflect.*
    s.asTerm match
      case Inlined(_, _, Literal(StringConstant(str))) =>
        Name(str).fold(report.errorAndAbort, _ => s)
      case _ => report.errorAndAbort("not constant")
