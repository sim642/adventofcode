package eu.sim642.adventofcode2020

object Day9 {

  def firstInvalid(numbers: Seq[Long], preambleLength: Int = 25): Long = {
    numbers
      .sliding(preambleLength + 1)
      .find({ case preamble :+ number =>
        !preamble.combinations(2).map(_.sum).contains(number)
      })
      .get
      .last
  }


  def parseNumbers(input: String): Seq[Long] = input.linesIterator.map(_.toLong).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(firstInvalid(parseNumbers(input)))
  }
}
