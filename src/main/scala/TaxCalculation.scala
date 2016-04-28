
/**
  * Created by marius on 23.04.16.
  */

sealed abstract class ShareTransaction {
    val shareName: String
    val isin: String
    val securityType = "Share"
    val fees: BigDecimal
    val date: String
    val currency: String
    val exchangeRate: BigDecimal
    val amount: Long
    val price: BigDecimal
    def totalPrice = amount * price
    def priceInLocalCurrency = price * exchangeRate
    def feesPrShare = fees / amount
    def priceInLocalCurrencyIncludingFees: BigDecimal
    def totalPriceInLocalCurrency = totalPrice * exchangeRate

    override def toString = {
        s"Date: $date, amount: $amount, price: $price, currency: $currency, price Local currency: $priceInLocalCurrency," +
          s" price per share including fees: $priceInLocalCurrencyIncludingFees, total in local currency: $totalPriceInLocalCurrency"
    }
}

case class ShareSale(date: String, currency: String = "NOK", exchangeRate: BigDecimal = 1, amount: Long, price: BigDecimal = 1, fees: BigDecimal = 0,
                     shareName: String = "", isin: String = "") extends ShareTransaction {
    override def toString = "Sale: " + super.toString
    override def totalPrice = super.totalPrice - fees
    override def priceInLocalCurrencyIncludingFees = (price - feesPrShare) * exchangeRate
}

case class ShareBuy(date: String, currency: String = "NOK", exchangeRate: BigDecimal = 1, amount: Long, price: BigDecimal = 1, fees: BigDecimal = 0,
                    shareName: String = "", isin: String = "") extends ShareTransaction {
    override def toString = "Buy: " + super.toString
    override def totalPrice = super.totalPrice + fees
    override def priceInLocalCurrencyIncludingFees = (price + feesPrShare) * exchangeRate
}

case class ShareRealisation(buy: ShareBuy, sale: ShareSale) {
    def gain = sale.totalPrice - buy.totalPrice
    def gainInLocalCurrency = sale.totalPriceInLocalCurrency - buy.totalPriceInLocalCurrency

    override def toString = {
        s"Bought: $buy\nSold: $sale\nGain: $gain. In local currency: $gainInLocalCurrency"
    }
}

case class Result(realisations: Seq[ShareRealisation] = Seq.empty, remainingShares: Seq[ShareBuy] = Seq.empty) {
    override def toString = {
        s"Realisations:\n${realisations.mkString("\n")}" +
          s"\n\nRemaining shares:\n${remainingShares.mkString("\n")}"
    }
}

object TaxCalculation {
    private def calculateRealisation(buy: ShareBuy, sale: ShareSale): (ShareRealisation, Option[ShareTransaction]) = {
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
        val res = sales.foldLeft(Result(remainingShares = buys))(handle)
        res.copy(realisations = res.realisations.reverse, res.remainingShares.reverse)
    }
}

