// 3https://qiita.com/yasuabe2613/items/3c3220d7005678822c25

import squants.mass.MassConversions._

val m1 = 185.pounds
// m1: Mass = 185.0 lb
val (q, u) = m1.toTuple // 必要なら数値と単位文字列に分けることもできる
// q: Double = 185.0
// u: String = lb

import squants.market.MoneyConversions._

val s5 = 5.dollars
// s5: Money = 5 USD

// Analysis Patterns 3.2
import squants.space.LengthConversions._
import squants.space.{Inches, Feet, Millimeters}

val f2i = 1.feet in Inches
// f2i: Length = 12.0 in

val i2f = 1.inches in Feet
// i2f: Length = 0.08333333333333334 ft

val i2mm = 1.inches in Millimeters // 25.4000508 mm
val f2mm = 1.feet   in Millimeters // 304.8006096 mm

// Analysis Patterns 3.3
import squants.space.AreaConversions._

150.squareYards // 150.0 yd²

150.squareYards / 15.squareYards // 10.0
150.squareYards / 15.yards       // 10.0 yd
150.squareYards / 15             // 10.0 yd²
150.squareYards / 1.meters       // 137.16027431999998 yd
// 150.squareYards / 1.grams     // コンパイルできない

import squants.motion.{Acceleration, EarthGravities}
import squants.motion.AccelerationConversions._
import squants.time.TimeConversions._

val g = EarthGravities(1)     // 重力加速度
// g: Acceleration = 1.0 g

given Acceleration = 0.01.mpss
g ≈ 9.81.mpss
// res5: Boolean = true

val t = 2.seconds
val v = g * t                 // 速度
// v: Velocity = 19.6133 m/s

val d = g * t.squared / 2// 距離
// d: Length = 19.6133 m

val f = g * 10.kilograms     // 力
// f: Force = 98.06649999999999 N
