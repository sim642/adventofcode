package eu.sim642.mits2024

import eu.sim642.adventofcodelib.IteratorImplicits.*

object Day1 {

  case class Input(kelluke: BigDecimal, forMe: BigDecimal, dynamit: BigDecimal, order: String) {
    def orderTotal: BigDecimal = {
      val it = """(Kelluke|ForMe|Dynamit)""".r.findAllMatchIn(order)
      val freqs = it.groupCount(_.matched).withDefaultValue(0)
      freqs("Kelluke") * kelluke + freqs("ForMe") * forMe + freqs("Dynamit") * dynamit
    }
  }

  def parseInput(input: String): Input = input match {
    case s"Kelluke: $kelluke\nForMe: $forMe\nDynamit: $dynamit\n$order" => Input(BigDecimal(kelluke), BigDecimal(forMe), BigDecimal(dynamit), order)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(parseInput(input).orderTotal)
  }
}
