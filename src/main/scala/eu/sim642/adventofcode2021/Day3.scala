package eu.sim642.adventofcode2021

import scala.annotation.tailrec

object Day3 {

  type Binary = Seq[Boolean]

  def binary2int(binary: Binary): Int = {
    binary.foldLeft(0)({ case (acc, bit) => (acc << 1) | (if bit then 1 else 0) })
  }

  def mostCommonBit(bits: Seq[Boolean]): Boolean = {
    2 * bits.count(_ == true) >= bits.size // multiply instead of dividing to avoid truncation
  }

  def gammaRate(binaries: Seq[Binary]): Binary = {
    binaries.transpose.map(mostCommonBit)
  }

  def powerConsumption(binaries: Seq[Binary]): Int = {
    val gamma = gammaRate(binaries)
    val epsilon = gamma.map(!_) // least common bits are the opposite
    binary2int(gamma) * binary2int(epsilon)
  }


  type BitCriteria = Seq[Boolean] => Boolean => Boolean

  def filterBitCriteria(binaries: Seq[Binary], bitCriteria: BitCriteria): Binary = {

    @tailrec
    def helper(binaries: Seq[Binary], i: Int): Binary = {
      val p = bitCriteria(binaries.map(_(i)))
      val newBinaries = binaries.filter(b => p(b(i)))
      newBinaries match {
        case Seq(rating) => rating
        case _ => helper(newBinaries, i + 1)
      }
    }

    helper(binaries, 0)
  }

  def oxygenGeneratorRatingBitCriteria(bits: Seq[Boolean]): Boolean => Boolean = {
    val mostCommon = mostCommonBit(bits)
    _ == mostCommon
  }

  def co2ScrubberRatingBitCriteria(bits: Seq[Boolean]): Boolean => Boolean = {
    val leastCommon = !mostCommonBit(bits)
    _ == leastCommon
  }

  def oxygenGeneratorRating(binaries: Seq[Binary]): Binary = filterBitCriteria(binaries, oxygenGeneratorRatingBitCriteria)

  def co2ScrubberRating(binaries: Seq[Binary]): Binary = filterBitCriteria(binaries, co2ScrubberRatingBitCriteria)

  def lifeSupportRating(binaries: Seq[Binary]): Int = {
    val oxygenGenerator = oxygenGeneratorRating(binaries)
    val co2Scrubber = co2ScrubberRating(binaries)
    binary2int(oxygenGenerator) * binary2int(co2Scrubber)
  }


  def parseBinary(s: String): Binary = s.map(_ == '1')

  def parseBinaries(input: String): Seq[Binary] = input.linesIterator.map(parseBinary).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(powerConsumption(parseBinaries(input)))
    println(lifeSupportRating(parseBinaries(input)))
  }
}
