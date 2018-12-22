package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2017.Day21.GridOps

import scala.collection.mutable

object Day22 {

  def calculateErosionLevel(depth: Int, target: Pos): Grid[Int] = {
    val erosionLevel = mutable.Seq.fill(target.y + 1, target.x + 1)(-1)
    for (y <- 0 to target.y) {
      for (x <- 0 to target.x) {
        val geologicIndex = (y, x) match {
          case (0, 0) => 0
          case (y, x) if x == target.x && y == target.y => 0
          case (0, x) => x * 16807
          case (y, 0) => y * 48271
          case (y, x) => erosionLevel(y)(x - 1) * erosionLevel(y - 1)(x)
        }

        erosionLevel(y)(x) = (geologicIndex + depth) % 20183
      }
    }
    erosionLevel.toVector.map(_.toVector)
  }

  def totalRiskLevel(depth: Int, target: Pos): Int = {
    val erosionLevel: Grid[Int] = calculateErosionLevel(depth, target)
    val riskLevel: Grid[Int] = erosionLevel.mapGrid(_ % 3)
    riskLevel.map(_.sum).sum
  }

  def totalRiskLevel(input: String): Int = {
    val (depth, target) = parseInput(input)
    totalRiskLevel(depth, target)
  }

  private val inputRegex =
    """depth: (\d+)
      |target: (\d+),(\d+)""".stripMargin.r

  def parseInput(input: String): (Int, Pos) = input match {
    case inputRegex(depth, targetX, targetY) => (depth.toInt, Pos(targetX.toInt, targetY.toInt))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day22.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(totalRiskLevel(input))
  }
}
