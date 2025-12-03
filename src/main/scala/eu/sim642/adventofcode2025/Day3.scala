package eu.sim642.adventofcode2025

object Day3 {

  type Bank = String

  def maxJoltage(bank: Bank): Int = {
    // TODO: doesn't keep order
    //bank.combinations(2).map(_.toInt).max
    // TODO: can be made linearly?
    (for {
      i <- bank.indices
      j <- (i + 1) until bank.length
    } yield s"${bank(i)}${bank(j)}".toInt).max
  }

  def totalJoltage(banks: Seq[Bank]): Int = banks.map(maxJoltage).sum

  def parseBanks(input: String): Seq[Bank] = input.linesIterator.toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalJoltage(parseBanks(input)))

    // part 1: 16769 - not right (combinations don't keep order)
  }
}
