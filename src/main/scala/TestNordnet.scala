import scala.io.{Codec, Source}

/**
  * Created by marius on 23.04.16.
  */
object TestNordnet extends App {
    val inputLines = Source.fromFile(args.head)(Codec.ISO8859).getLines()
    val trans = NordnetImporter.parseCSVString(inputLines)

    println(s"Name: ${trans.headOption.map(_.shareName).getOrElse("N/A")} ISIN: ${trans.headOption.map(_.isin).getOrElse("no transactions")}\n" +
      s"Trans:\n" + trans.toList.mkString("\n"))

    val res = TaxCalculation.calculateRealisations(trans)
    println(res)
}
