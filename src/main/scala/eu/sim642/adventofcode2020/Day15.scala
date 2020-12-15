package eu.sim642.adventofcode2020

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 {

  def simulateNumber(startingNumbers: Seq[Int], returnIndex: Int = 2020): Int = {
    // mutable.Map because immutable.Map is too slow
    // LongMap because it avoids key boxing
    val numberIndices =
      startingNumbers
        .map(_.toLong)
        .init
        .zipWithIndex
        .to(mutable.LongMap)

    @tailrec
    def helper(i: Int, prevNumber: Int): Int = {
      if (i == returnIndex)
        prevNumber
      else {
        val prevI = i - 1
        val number = prevI - numberIndices.getOrElse(prevNumber, prevI)
        numberIndices += prevNumber.toLong -> prevI
        helper(i + 1, number)
      }
    }

    helper(startingNumbers.length, startingNumbers.last)
  }

  // TODO: optimize part 2
  val part2ReturnIndex: Int = 30000000


  def parseStartingNumbers(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim
  val input: String = "8,0,17,4,1,12"

  def main(args: Array[String]): Unit = {
    println(simulateNumber(parseStartingNumbers(input)))
    println(simulateNumber(parseStartingNumbers(input), returnIndex = part2ReturnIndex))
  }
}
