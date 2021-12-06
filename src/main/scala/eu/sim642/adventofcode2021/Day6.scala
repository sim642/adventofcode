package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day6 {

  type State = Vector[Long] // used as Map, where index is timer and value is count

  def input2state(input: Seq[Int]): State = Vector.tabulate(9)(i => input.count(_ == i)) // largest value is 8

  def stepState(state: State): State = {
    val zeroCount +: stateNonZero = state
    stateNonZero.updated(6, stateNonZero(6) + zeroCount) :+ zeroCount
  }

  def countLanternfish(input: Seq[Int], after: Int = 80): Long = {
    Iterator.iterate(input2state(input))(stepState)(after).sum
  }


  def parseInput(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countLanternfish(parseInput(input)))
    println(countLanternfish(parseInput(input), 256))
  }
}
