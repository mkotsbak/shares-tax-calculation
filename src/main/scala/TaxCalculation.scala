
/**
  * Created by marius on 23.04.16.
  */

abstract class ShareTransaction {
    val date: String
    val currency: String
    val exchangeRate: BigDecimal
    val amount: BigDecimal
}

case class ShareSale(date: String, currency: String = "NOK", exchangeRate: BigDecimal = 1, amount: BigDecimal) extends ShareTransaction
case class ShareBuy(date: String, currency: String = "NOK", exchangeRate: BigDecimal = 1, amount: BigDecimal) extends ShareTransaction

case class ShareRealisation(buy: ShareBuy, sale: ShareSale)
case class Result(realisations: Seq[ShareRealisation], remainingShares: Seq[ShareBuy])

object TaxCalculation {
    def calculateRealisations(transactions: Seq[ShareTransaction]): Result = {
        val buys = transactions.collect { case s: ShareBuy => s }
        val sales = transactions.collect { case s: ShareSale => s }

        //Result(Seq.empty, Seq.empty)
        Result(
            realisations = Seq(
                ShareRealisation(buys.head, sales.head)
            ),
            remainingShares = Seq.empty
        )
    }
}

object TestTaxCalculation extends App {
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
}
