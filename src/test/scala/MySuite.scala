class MySuite extends munit.FunSuite {
  test("Problem 1 example data") {
    val result = getBestGroupPrices(
      Seq(
        Rate("M1", "Military"),
        Rate("M2", "Military"),
        Rate("S1", "Senior"),
        Rate("S2", "Senior")
      ),
      Seq(
        CabinPrice("CA", "M1", 200.00),
        CabinPrice("CA", "M2", 250.00),
        CabinPrice("CA", "S1", 225.00),
        CabinPrice("CA", "S2", 260.00),
        CabinPrice("CB", "M1", 230.00),
        CabinPrice("CB", "M2", 260.00),
        CabinPrice("CB", "S1", 245.00),
        CabinPrice("CB", "S2", 270.00)
      )
    )
    checkProblemOne(
      Seq(
        BestGroupPrice("CA", "M1", 200.00, "Military"),
        BestGroupPrice("CA", "S1", 225.00, "Senior"),
        BestGroupPrice("CB", "M1", 230.00, "Military"),
        BestGroupPrice("CB", "S1", 245.00, "Senior")
      ),
      result
    )
  }

  test("Problem 2 example data") {
    val promotions = Seq(
      Promotion("P1", Seq("P3")),
      Promotion("P2", Seq("P4", "P5")),
      Promotion("P3", Seq("P1")),
      Promotion("P4", Seq("P2")),
      Promotion("P5", Seq("P2"))
    )
    val result = allCombinablePromotions(promotions)
    checkProblemTwo(
      Seq(
        PromotionCombo(Seq("P1", "P2")),
        PromotionCombo(Seq("P1", "P4", "P5")),
        PromotionCombo(Seq("P2", "P3")),
        PromotionCombo(Seq("P3", "P4", "P5"))
      ),
      result
    )
  }

  def checkProblemOne(
      expected: Seq[BestGroupPrice],
      actual: Seq[BestGroupPrice]
  ) =
    expected.foreach(e =>
      assert(actual.contains(e), s"Result missing expected '$e'")
    )

  def checkProblemTwo(
      expected: Seq[PromotionCombo],
      actual: Seq[PromotionCombo]
  ) =
    val exp = expected.map(e => e.promotionCodes.toSet)
    val act = actual.map(a => a.promotionCodes.toSet)
    exp.foreach(e => assert(act.contains(e), s"Result missing expected '$e'"))
}
