package functional_tdd

import java.time.LocalTime
import cats.Id
import cats.syntax.apply.*
import cats.syntax.functor.*
import munit.ScalaCheckSuite
import org.scalacheck.{ Arbitrary, Gen }
import org.scalacheck.Prop.*
import org.scalacheck.cats.implicits.*

class GreetSuit extends ScalaCheckSuite:
  import greet.*

  test("与えられた時刻があいさつに正しく対応付けられる") {
    List(
      // 時 | 分 | 秒 | ナノ      | 挨拶
      ((  0 ,  0 ,  0 ,         0), "こんばんは"        ),
      ((  4 , 59 , 59 , 999999999), "こんばんは"        ),
      ((  5 ,  0 ,  0 ,         0), "おはようございます"),
      (( 11 , 59 , 59 , 999999999), "おはようございます"),
      (( 12 ,  0 ,  0 ,         0), "こんにちは"        ),
      (( 17 , 59 , 59 , 999999999), "こんにちは"        ),
      (( 18 ,  0 ,  0 ,         0), "こんばんは"        ),
      (( 23 , 59 , 59 , 999999999), "こんばんは"        ),
    ) foreach { case ((h, m, s, n), expected) =>
      val t = LocalTime.of(h, m, s, n)
      assertEquals(greetingOf(t), (expected))
    }
  }
  property("現在時刻があいさつに正しく対応付けられる") {
    given Arbitrary[LocalTime] = Arbitrary {
      val hourGen = Gen.choose(0, 23)
      val minGen  = Gen.choose(0, 59)
      val secGen  = Gen.choose(0, 59)
      val nanoGen = Gen.choose(0, 999999999)
      (hourGen, minGen, secGen, nanoGen) mapN LocalTime.of
    }
    forAll { (t: LocalTime) =>
      given Now[Id] with
        def time: Id[LocalTime] = t

      greetNow[Id] == greetingOf(t)
    }
  }