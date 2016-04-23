import java.util.Locale

/**
  * Created by marius on 23.04.16.
  */

trait CSVImporter {
    def importFromCSV(header: Seq[String], csvInput: Seq[Seq[String]]): Seq[ShareTransaction]

    def parseCSVString(csvInput: String): Seq[ShareTransaction] = {
        val res = csvInput.replace(',', '.') .lines.
          filter(_.trim.length > 0).
          map(_.split(';').toSeq).toSeq

        importFromCSV(res.head, res.tail)
    }
}
object NordnetImporter extends CSVImporter {
    override def importFromCSV(header: Seq[String], csvInput: Seq[Seq[String]]): Seq[ShareTransaction] = {
        csvInput.reverse.flatMap { line =>
            def value(field: String) = line(header.indexOf(field))

            val count = value("Antall").toLong
            val date = value("Handelsdag")
            val fees = BigDecimal(value("Avgifter").toDouble)
            val exchangeRate = BigDecimal(value("Vekslingskurs"))
            val price = BigDecimal(value("Kurs").replace(" ", ""))
            val currency = value("Valuta")

            val kjopt = "KJ.PT".r
              value("Transaksjonstype") match {
                case kjopt() => Seq(
                    ShareBuy(date = date, price = price, amount = count, fees = fees, exchangeRate = exchangeRate, currency = currency)
                )
                case "SALG" => Seq(
                    ShareSale(date = date, price = price, amount = count, fees = fees, exchangeRate = exchangeRate, currency = currency)
                )
                case _ => Seq.empty
            }
        }
    }
}
