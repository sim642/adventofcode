package eu.sim642.adventofcode2020

import scala.annotation.tailrec
import scala.collection.mutable

object Day15 {

  sealed trait Part {
    protected val defaultReturnIndex: Int

    def simulateNumber(startingNumbers: Seq[Int], returnIndex: Int = defaultReturnIndex): Int = {
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
  }

  object Part1 extends Part {
    override protected val defaultReturnIndex: Int = 2020
  }

  object Part2 extends Part {
    override protected val defaultReturnIndex: Int = 30000000
  }


  def parseStartingNumbers(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim
  val input: String = "8,0,17,4,1,12"

  def main(args: Array[String]): Unit = {
    println(Part1.simulateNumber(parseStartingNumbers(input)))
    println(Part2.simulateNumber(parseStartingNumbers(input)))
  }
}
