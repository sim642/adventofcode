package eu.sim642.adventofcode2020

import scala.annotation.tailrec

object Day15 {

  def simulateNumber(startingNumbers: Seq[Int], returnIndex: Int = 2020): Int = {

    @tailrec
    def helper(i: Int, numberIndices: Map[Int, Int], prevNumber: Int): Int = {
      //println(i, numberIndices, prevNumber)
      if (i == returnIndex)
        prevNumber
      else if (numberIndices.contains(prevNumber)) {
        val prevNumberIndex = numberIndices(prevNumber)
        val number = i - 1 - prevNumberIndex
        helper(i + 1, numberIndices + (prevNumber -> (i - 1)), number)
      }
      else
        helper(i + 1, numberIndices + (prevNumber -> (i - 1)), 0)
    }

    helper(startingNumbers.length, startingNumbers.init.zipWithIndex.toMap, startingNumbers.last)
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
