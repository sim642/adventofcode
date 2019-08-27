package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day1 {

  def iterateFloors(instructions: String): Iterator[Int] = {
    instructions.iterator.scanLeft(0)({ case (floor, instruction) =>
      instruction match {
        case '(' => floor + 1
        case ')' => floor - 1
        case _ => ???
      }
    })
  }

  def finalFloor(instructions: String): Int = {
    iterateFloors(instructions).last
  }

  def basementPosition(instructions: String): Int = {
    iterateFloors(instructions).indexOf(-1)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day1.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(finalFloor(input))
    println(basementPosition(input))
  }
}
