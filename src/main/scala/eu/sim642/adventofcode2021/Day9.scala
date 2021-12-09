package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.pos.Pos

object Day9 {

  def sumLowPointRiskLevels(grid: Grid[Int]): Int = {
    val lowPointHeights = for {
      (row, y) <- grid.zipWithIndex
      (cell, x) <- row.zipWithIndex
      pos = Pos(x, y)
      neighbors = Pos.axisOffsets.map(pos + _).filter(grid.containsPos)
      if neighbors.forall(grid(_) > cell)
    } yield cell
    lowPointHeights.map(_ + 1).sum
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector.map(_.asDigit)).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumLowPointRiskLevels(parseGrid(input)))
  }
}
