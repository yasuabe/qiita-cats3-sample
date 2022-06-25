import squants.market.{JPY, MoneyContext, USD}
import squants.market.MoneyConversions.*

val rate = USD / 112.14.yen
// r: CurrencyExchangeRate = USD/JPY 112.14

rate convert 10.dollars
// 1121.4 JPY

import squants.market.defaultMoneyContext
given MoneyContext =
  defaultMoneyContext withExchangeRates List(USD / 112.14.yen)

10.dollars in JPY
// 1121.4 JPY

import squants.space.VolumeConversions.*

// 原油価格 1バレル（４２ガロン） につき 64.39 USドル
val oilPrice = 64.39.dollars / 42.gallons
// oilPrice: Price[Volume] = 64.39 USD/42.0 gal

// これを1000リットル買うと日本円で、、、
(oilPrice * 1000.liters in JPY).toFormattedString
// res2: String = ¥45417
