import scala.util.{Failure, Try}
import cats.syntax.either.*
import cats.syntax.apply.*
import io.circe.*
import io.circe.generic.auto.*
import io.circe.parser.parse
import io.circe.refined.*
import eu.timepit.refined.types.string.NonEmptyString
import squants.market.{Money, Price}
import squants.space.Volume
import squants.market.MoneyContext
import squants.market.defaultMoneyContext

val rawJson: String = """{
  |  "name":      "crude oil",
  |  "quantity":  "456 L",
  |  "unitPrice": "64.39 USD/42 gal"
  }""".stripMargin

case class Order(
  name:      NonEmptyString,
  quantity:  Volume,
  unitPrice: Price[Volume]
)

def makeDecoder[A](f: String => Try[A]): Decoder[A] = Decoder.decodeString.emap {
  f(_).toEither.leftMap(_.getMessage)
}
given Decoder[Volume]        = makeDecoder(Volume(_))
given Decoder[Price[Volume]] =
  given MoneyContext = defaultMoneyContext
  val priceUnit      = "([^/]*)/(.*)".r
  makeDecoder {
    case priceUnit(p, u) => (Money(p), Volume(u)) mapN (_ / _)
    case _               => Failure(Exception("cannot parse unit price"))
  }

val Right(order) = parse(rawJson) flatMap (_.as[Order])