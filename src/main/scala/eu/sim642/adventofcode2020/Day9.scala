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

  def contiguousSumRange(numbers: Seq[Long], sum: Long): Seq[Long] = {
    // TODO: optimize instead of naive combinations
    val sumNumbers = numbers.scanLeft(0L)(_ + _)
    sumNumbers
      .zipWithIndex
      .combinations(2)
      .collectFirst({ case Seq((sum1, i1), (sum2, i2)) if i1 + 1 < i2 && sum1 + sum == sum2 =>
        numbers.slice(i1, i2)
      })
      .get
  }

  def encryptionWeakness(numbers: Seq[Long], preambleLength: Int = 25): Long = {
    val invalid = firstInvalid(numbers, preambleLength)
    val range = contiguousSumRange(numbers, invalid)
    range.min + range.max
  }


  def parseNumbers(input: String): Seq[Long] = input.linesIterator.map(_.toLong).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(firstInvalid(parseNumbers(input)))
    println(encryptionWeakness(parseNumbers(input)))
  }
}
