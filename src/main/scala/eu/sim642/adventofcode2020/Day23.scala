package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.SeqImplicits._
import eu.sim642.adventofcodelib.IntegralImplicits._

object Day23 {

  type Cups = Vector[Int]

  def simulateMove(cups: Cups): Cups = {
    val (current +: picked, rest) = cups.splitAt(4)
    val destination = Iterator.from(current - 1, -1).map(_ %+ 10).find(rest.contains).get
    val (beforeDestination, afterDestination) = rest.splitAt(rest.indexOf(destination) + 1)
    val newRest = beforeDestination ++ picked ++ afterDestination
    (current +: newRest).rotateLeft(1)
  }

  def simulateMoves(cups: Cups, moves: Int = 100): Cups = {
    Iterator.iterate(cups)(simulateMove)(moves)
  }

  def simulateMovesLabels(cups: Cups, moves: Int = 100): String = {
    val finalCups = simulateMoves(cups, moves)
    val 1 +: restCups = finalCups.rotateLeft(finalCups.indexOf(1))
    restCups.mkString("")
  }


  def parseCups(input: String): Cups = input.map(_.asDigit).toVector

  //lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim
  val input: String = "318946572"

  def main(args: Array[String]): Unit = {
    println(simulateMovesLabels(parseCups(input)))
  }
}
