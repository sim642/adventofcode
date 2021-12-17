package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day17 {

  def simulateY(initialYVelocity: Int): Iterator[Int] = {
    Iterator.iterate((0, initialYVelocity))((y, yVelocity) => (y + yVelocity, yVelocity - 1)).map(_._1)
  }

  def hitsTargetY(target: Box, initialYVelocity: Int): Boolean = {
    simulateY(initialYVelocity).takeWhile(_ >= target.min.y).exists(_ <= target.max.y)
  }

  def findHighestY(target: Box): Int = {
    (-100 to 100)
      .filter(hitsTargetY(target, _))
      .map(initialYVelocity => simulateY(initialYVelocity).takeWhile(_ >= target.min.y).max)
      .max
  }


  private val targetRegex = """target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)""".r

  def parseTarget(input: String): Box = input match {
    case targetRegex(xMin, xMax, yMin, yMax) =>
      Box(Pos(xMin.toInt, yMin.toInt), Pos(xMax.toInt, yMax.toInt))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findHighestY(parseTarget(input)))
  }
}
