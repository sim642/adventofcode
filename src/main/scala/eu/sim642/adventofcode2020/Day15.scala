package eu.sim642.adventofcode2020

import scala.annotation.tailrec

object Day15 {

  sealed trait Part {
    protected val defaultReturnIndex: Int

    def simulateNumber(startingNumbers: Seq[Int], returnIndex: Int = defaultReturnIndex): Int = {
      // primitive array because immutable.Map, mutable.Map and LongMap are slower
      val numberIndices: Array[Int] = Array.ofDim(returnIndex) // 0-initialized by default
      startingNumbers
        .init
        .zipWithIndex
        .foreach({ case (number, i) =>
          numberIndices(number) = i + 1 // 1-indexed to avoid conflict with 0-initialization
        })

      @tailrec
      def helper(i: Int, prevNumber: Int): Int = {
        if (i == returnIndex)
          prevNumber
        else {
          val number = numberIndices(prevNumber) match {
            case 0 => 0
            case prevNumberI => i - prevNumberI
          }
          numberIndices(prevNumber) = i
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
