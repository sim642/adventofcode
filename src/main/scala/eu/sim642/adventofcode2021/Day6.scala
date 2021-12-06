package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.IterableImplicits._
import eu.sim642.adventofcodelib.IteratorImplicits._

object Day6 {

  type State = Map[Int, Long]

  def input2state(input: Seq[Int]): State = input.groupCount(identity).view.mapValues(_.toLong).toMap

  def stepState(state: State): State = {
    val stateNonZero = (state - 0).map({ case (timer, count) => (timer - 1) -> count })
    val zeroCount = state.getOrElse(0, 0L)
    //val stateZero = Map(6 -> zeroCount, 8 -> zeroCount)
    // TODO: clean up
    stateNonZero.updatedWith(6)(countOpt => Some(countOpt.getOrElse(0L) + zeroCount)).updatedWith(8)(countOpt => Some(countOpt.getOrElse(0L) + zeroCount)).filter(_._2 != 0)
  }

  def countLanternfish(input: Seq[Int], after: Int = 80): Long = {
    Iterator.iterate(input2state(input))(stepState)(after).valuesIterator.sum
  }


  def parseInput(input: String): Seq[Int] = input.split(",").toSeq.map(_.toInt)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countLanternfish(parseInput(input)))
    println(countLanternfish(parseInput(input), 256))
  }
}
