import java.util.Locale

/**
  * Created by marius on 23.04.16.
  */

trait CSVImporter {
    def importFromCSV(header: Seq[String], csvInput: Seq[Seq[String]]): Seq[ShareTransaction]

    def parseCSVString(csvInput: Iterator[String]): Seq[ShareTransaction] = {
        val res = csvInput.map(_.replace(',', '.')).
          filter(_.trim.length > 0).
          map(_.split(';').toSeq).toSeq

        importFromCSV(res.head, res.tail)
    }
}
object NordnetImporter extends CSVImporter {
    override def importFromCSV(header: Seq[String], csvInput: Seq[Seq[String]]): Seq[ShareTransaction] = {
        csvInput.reverse.flatMap { line =>
            def value(field: String) = line(header.indexOf(field))

            val shareName = value("Verdipapir")
            val isin = value("ISIN")
            val count = value("Antall").toLong
            val date = value("Handelsdag")
            val fees = BigDecimal(value("Avgifter").toDouble)
            val exchangeRate = BigDecimal(value("Vekslingskurs"))
            val price = BigDecimal(value("Kurs").replace(" ", ""))
            val currency = value("Valuta")

            val kjopt = "KJ.PT".r
              value("Transaksjonstype") match {
                case kjopt() => Seq(
                    ShareBuy(shareName = shareName, date = date, price = price, amount = count, fees = fees,
                      exchangeRate = exchangeRate, currency = currency, isin = isin)
                )
                case "SALG" => Seq(
                    ShareSale(shareName = shareName, date = date, price = price, amount = count, fees = fees,
                      exchangeRate = exchangeRate, currency = currency, isin = isin)
                )
                case _ => Seq.empty
            }
        }
    }
}
