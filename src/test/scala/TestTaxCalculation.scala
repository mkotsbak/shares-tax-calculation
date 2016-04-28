import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by marius on 28.04.16.
  */
class TestTaxCalculation extends FlatSpec with Matchers {
    "The Tax calculator" should "support simple cases" in {
        val input1 = Seq(
            ShareBuy(
                date = "2015-01-01",
                amount = 100
            ),
            ShareSale(
              date = "2015-02-01",
              amount = 100
            )
        )

        val output = TaxCalculation.calculateRealisations(input1)
        val realisations = output.realisations.headOption
        assert(realisations.isDefined)
        assert(realisations.get.sale.amount == 100 )
        assert (output.remainingShares.isEmpty)

        println(s"TestSimple: \nInput: $input1: \noutput: $output\n\n")
    }

    it should "support buy split remaining" in {
        val input1 = Seq(
            ShareBuy(
                date = "2015-01-01",
                amount = 100,
                price = 100.0
            ),
            ShareSale(
                date = "2015-02-01",
                amount = 50,
                price = 110.0
            )
        )

        val output = TaxCalculation.calculateRealisations(input1)
        val realisations = output.realisations.headOption
        assert(realisations.isDefined)
        assert(realisations.get.sale.amount == 50 )
        assert(realisations.get.buy.amount == 50 )
        assert(output.remainingShares.nonEmpty)

        println(s"TestBuySplitRemaining: \nInput: $input1: \noutput: $output\n\n")
    }

    it should "support sale split remaining" in {
        val input1 = Seq(
            ShareBuy(
                date = "2015-01-01",
                amount = 60
            ),
            ShareBuy(
                date = "2015-02-01",
                amount = 40
            ),
            ShareSale(
                date = "2015-03-01",
                amount = 100
            )
        )

        val output = TaxCalculation.calculateRealisations(input1)
        val realisation = output.realisations.headOption
        assert(realisation.isDefined)
        assert(realisation.get.sale.amount == 60 )
        assert(realisation.get.buy.amount == 60 )
        assert(output.remainingShares.isEmpty)
        assert(output.realisations.size == 2)

        println(s"TestSaleSplitRemaining: \nInput: $input1: \noutput: $output\n\n")
    }
}
