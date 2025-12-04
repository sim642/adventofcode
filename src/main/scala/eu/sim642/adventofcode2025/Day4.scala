package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.pos.Pos

object Day4 {

  // TODO: make this more like cellular automaton?
  def accessibleRolls(grid: Grid[Char]): Seq[Pos] = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      pos = Pos(x, y)
      if grid(pos) == '@'
      neighbors = Pos.allOffsets.map(pos + _).filter(grid.containsPos)
      if neighbors.count(grid(_) == '@') < 4
    } yield pos).toSeq
  }

  def countAccessibleRolls(grid: Grid[Char]): Int = {
    accessibleRolls(grid).size
  }

  def countRemovableRolls(grid: Grid[Char]): Int = {
    val accessible = accessibleRolls(grid)
    if (accessible.isEmpty)
      0
    else {
      val newGrid = accessible.foldLeft(grid)(_.updatedGrid(_, '.'))
      accessible.size + countRemovableRolls(newGrid)
    }
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countAccessibleRolls(parseGrid(input)))
    println(countRemovableRolls(parseGrid(input)))
  }
}
