import cats.arrow.Profunctor
import cats.data.Kleisli
import monocle.Lens
import monocle.macros.GenLens
import cats.syntax.either.*

// simple exsample
val fab: Double => Double = x => x + 0.3

val f: Int    => Double = x => x.toDouble / 2
val g: Double => Double = x => x * 3
val h = Profunctor[Function1].dimap(fab)(f)(g)
h(3) //res0: Double = 5.4

// application to accessors
import _root_.profunctor_sample.Name

import Name.*

case class Person(name: Name)

val p1 = Person(Name.from("test"))
val p2 = Person(Name.from("testfoo"))

type ErrorOr[A] = Either[String, A]

val stripTest: Kleisli[ErrorOr, Name, Name] =
  Kleisli(name => Name(name.stripPrefix("test")))

val nameLens = GenLens[Person](_.name)

def dimapped(p: Person) = stripTest.dimap(nameLens.get)(nameLens.replace(_)(p))

def stripTestFromName(p: Person) = dimapped(p).run(p)

stripTestFromName(p1) //Left(error)
stripTestFromName(p2) //Right(Person(foo))
stripTestFromName(Person(Name.from("abc"))) //Right(Person(abc))

2