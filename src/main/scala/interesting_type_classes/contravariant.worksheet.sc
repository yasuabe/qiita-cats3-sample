import cats.Contravariant
import cats.syntax.contravariant.*

case class Money(amount: Int)
case class Earnings(basic: Money)
case class Deductions(tax: Money)
case class Salary(earnings: Earnings, deductions: Deductions)

val salary: Salary = Salary(Earnings(Money(100)), Deductions(Money(10)))

trait Validate[A]:
  def validate(a: A): Boolean

given Contravariant[Validate] with
  def contramap[A, B](fa: Validate[A])(f: B => A) = b => fa validate f(b)

val nonNegative: Validate[Money] = _.amount >= 0

val basic            = (_: Earnings).basic
(nonNegative contramap basic) validate Earnings(Money(-100))

val earningsAmount   = (_: Salary).earnings.basic
val deductionsAmount = (_: Salary).deductions.tax
(nonNegative contramap earningsAmount) validate salary
(nonNegative contramap deductionsAmount) validate salary

val earnings         = (_: Salary).earnings
(nonNegative contramap basic contramap earnings) validate salary
