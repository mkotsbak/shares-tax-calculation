
/**
  * Created by marius on 23.04.16.
  */

abstract class ShareTransaction {
    val date: String
    val currency: String
    val exchangeRate: BigDecimal
    val amount: Long
    val price: BigDecimal
    def totalPrice = amount * price
    def priceInLocalCurrency = price * exchangeRate
    def totalPriceInLocalCurrency = amount * priceInLocalCurrency

    override def toString = {
        s"Date: $date, amount: $amount, price: $price, currency: $currency, price Local currency: $priceInLocalCurrency, total in local currency: $totalPriceInLocalCurrency"
    }
}

case class ShareSale(date: String, currency: String = "NOK", exchangeRate: BigDecimal = 1, amount: Long, price: BigDecimal = 1) extends ShareTransaction {
    override def toString = "Sale: " + super.toString
}

case class ShareBuy(date: String, currency: String = "NOK", exchangeRate: BigDecimal = 1, amount: Long, price: BigDecimal = 1) extends ShareTransaction {
    override def toString = "Buy: " + super.toString
}

case class ShareRealisation(buy: ShareBuy, sale: ShareSale) {
    def gain = sale.totalPrice - buy.totalPrice
    def gainInLocalCurrency = sale.totalPriceInLocalCurrency - buy.totalPriceInLocalCurrency

    override def toString = {
        s"Bought: $buy\nSold: $sale\nGain: $gain. In local currency: $gainInLocalCurrency"
    }
}

case class Result(realisations: Seq[ShareRealisation] = Seq.empty, remainingShares: Seq[ShareBuy] = Seq.empty)

object TaxCalculation {
    def calculateRealisation(buy: ShareBuy, sale: ShareSale): (ShareRealisation, Option[ShareTransaction]) = {
        if (buy.amount == sale.amount) ( ShareRealisation(buy, sale), None )
        else if (buy.amount > sale.amount) ( ShareRealisation(buy.copy(amount = sale.amount), sale),
            Some(buy.copy(amount = buy.amount - sale.amount))
          )
        else ( ShareRealisation(buy, sale.copy(amount = buy.amount)),
          Some(sale.copy(amount = sale.amount - buy.amount))
          )
    }

    def calculateRealisations(transactions: Seq[ShareTransaction]): Result = {
        val buys = transactions.collect { case s: ShareBuy => s }
        val sales = transactions.collect { case s: ShareSale => s }

        def handle(res: Result, sale: ShareSale): Result = {
            val (realisation, remaining) = calculateRealisation(buy = res.remainingShares.head, sale)
            remaining match {
                case None => res.copy(realisations = realisation +: res.realisations, remainingShares = res.remainingShares.tail)
                case Some(b: ShareBuy) => res.copy(realisations = realisation +: res.realisations, remainingShares = b +: res.remainingShares.tail)
                case Some(s: ShareSale) => handle(res.copy(realisations = realisation +: res.realisations,
                    remainingShares = res.remainingShares.tail), s)
            }
        }
        val res = sales.foldLeft(Result(remainingShares = buys.reverse))(handle)
        res.copy(realisations = res.realisations.reverse, res.remainingShares.reverse)
    }
}

object TestTaxCalculation extends App {
    def testSimple = {
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
    testSimple

    def testBuySplitRemaining = {
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
    testBuySplitRemaining

    def testSaleSplitRemaining = {
        val input1 = Seq(
            ShareBuy(
                date = "2015-01-01",
                amount = 50
            ),
            ShareBuy(
                date = "2015-02-01",
                amount = 50
            ),
            ShareSale(
                date = "2015-03-01",
                amount = 100
            )
        )

        val output = TaxCalculation.calculateRealisations(input1)
        val realisation = output.realisations.headOption
        assert(realisation.isDefined)
        assert(realisation.get.sale.amount == 50 )
        assert(realisation.get.buy.amount == 50 )
        assert(output.remainingShares.isEmpty)
        assert(output.realisations.size == 2)

        println(s"TestSaleSplitRemaining: \nInput: $input1: \noutput: $output\n\n")
    }
    testSaleSplitRemaining
}
