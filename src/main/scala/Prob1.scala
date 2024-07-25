import scala.collection.mutable.HashMap

object Prob1 {
  def main(args: Array[String]): Unit = {
    val rates = Seq(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )
    val result = getBestGroupPrices(rates, prices)
    println(result)
  }
}

case class Rate(rateCode: String, rateGroup: String)

case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

case class BestGroupPrice(
    cabinCode: String,
    rateCode: String,
    price: BigDecimal,
    rateGroup: String
)

def getBestGroupPrices(
    rates: Seq[Rate],
    prices: Seq[CabinPrice]
): Seq[BestGroupPrice] =
  // Build rate code to rate group mapping
  val rateMap = rates.foldLeft(HashMap.empty[String, String]) { (map, rate) =>
    map += (rate.rateCode -> rate.rateGroup)
  }

  val minPrices = prices
    // Augment prices with rate group mappings
    .map(price =>
      BestGroupPrice(
        price.cabinCode,
        price.rateCode,
        price.price,
        rateMap.getOrElse(
          price.rateCode,
          // Throwing exception here as I didn't want to alter the function signature to
          // return an error type like Either
          throw new NoSuchElementException(
            s"expected to find rate group for rate code '${price.rateCode}'"
          )
        )
      )
    )
    // Group prices by cabin and rate group, take cheapest
    .groupBy(price => s"${price.cabinCode}${price.rateGroup}")
    .mapValues(groupedPrices => groupedPrices.minBy(price => price.price))
    .values
    .toSeq

  return minPrices
