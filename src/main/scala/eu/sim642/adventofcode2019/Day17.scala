package eu.sim642.adventofcode2019

import eu.sim642.adventofcode2019.Day9._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits._

object Day17 {

  def sumAlignmentParameters(grid: Grid[Char]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '#'
      pos = Pos(x, y)
      neighbors = Pos.axisOffsets.map(pos + _).filter(grid.containsPos)
      if neighbors.forall(grid(_) == '#')
    } yield pos.x * pos.y).sum
  }

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseInput(input: String): Grid[Char] = {
    parseGrid(ProgramState(parseProgram(input)).outputs.map(_.toChar).mkString)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    printGrid(parseInput(input))
    println(sumAlignmentParameters(parseInput(input)))
  }
}
