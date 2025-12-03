package eu.sim642.adventofcode2025

import scala.collection.mutable

object Day3 {

  type Bank = String

  def maxJoltageDigits(bank: Bank, digits: Int): Long = {
    val memo = mutable.Map.empty[(Int, Int), Long]

    def helper(i: Int, digits: Int): Long = {
      memo.getOrElseUpdate((i, digits), {
        if (i >= 0 && digits >= 1) {
          helper(i - 1, digits) max (helper(i - 1, digits - 1) * 10 + bank(i).asDigit)
        } else
          0
      })
    }

    helper(bank.length - 1, digits)
  }

  trait Part {
    val digits: Int

    def maxJoltage(bank: Bank): Long = maxJoltageDigits(bank, digits)

    def totalJoltage(banks: Seq[Bank]): Long = banks.map(maxJoltage).sum
  }

  object Part1 extends Part {
    override val digits: Int = 2
  }

  object Part2 extends Part {
    override val digits: Int = 12
  }

  def parseBanks(input: String): Seq[Bank] = input.linesIterator.toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalJoltage(parseBanks(input)))
    println(Part2.totalJoltage(parseBanks(input)))

    // part 1: 16769 - not right (combinations don't keep order)
  }
}
