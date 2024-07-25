object Prob2 {
  def main(args: Array[String]): Unit = {
    val promotions = Seq(
      Promotion("P1", Seq("P3")), // P1 is not combinable with P3
      Promotion("P2", Seq("P4", "P5")), // P2 is not combinable with P4 and P5
      Promotion("P3", Seq("P1")), // P3 is not combinable with P1
      Promotion("P4", Seq("P2")), // P4 is not combinable with P2
      Promotion("P5", Seq("P2")) // P5 is not combinable with P2
    )
    println(allCombinablePromotions(promotions))
    println(combinablePromotions("P1", promotions))
    println(combinablePromotions("P3", promotions))
  }
}

case class Promotion(code: String, notCombinableWith: Seq[String])

case class PromotionCombo(promotionCodes: Seq[String])

def allCombinablePromotions(
    allPromotions: Seq[Promotion]
): Seq[PromotionCombo] =
  // Gather all distinct promotion codes
  val promotionItems = allPromotions.flatMap { promo =>
    promo.code +: promo.notCombinableWith
  }.distinct
  // Get all combinations of the promotion items
  val combinations =
    (1 to promotionItems.size).flatMap(promotionItems.combinations)
  // Get all excluded combinations from the passed promotions
  val excludedCombinations = allPromotions.flatMap(promo =>
    promo.notCombinableWith.map(str => Set(promo.code, str))
  )

  val filtered = combinations
    // Filter out excluded combinations
    .filter(seq => !excludedCombinations.exists(set => set.subsetOf(seq.toSet)))
    // Filter out redundant combination subsets
    .foldLeft(Seq.empty[Seq[String]]) { (acc, curr) =>
      // If the Seq being considered is already in the accumulator, leave unchanged
      if acc.exists(seq => curr.toSet.subsetOf(seq.toSet)) then acc
      // Otherwise, remove subsets of curr and add it to accumulator
      else acc.filterNot(seq => seq.toSet.subsetOf(curr.toSet)) :+ curr
    }
    .map(seq => PromotionCombo(seq))

  return filtered

def combinablePromotions(
    promotionCode: String,
    allPromotions: Seq[Promotion]
): Seq[PromotionCombo] =
  return allCombinablePromotions(allPromotions).filter(promo =>
    promo.promotionCodes.contains(promotionCode)
  )
