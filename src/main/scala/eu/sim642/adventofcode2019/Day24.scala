package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.pos.Pos

object Day24 {

  // inspired by 2018 Day 18
  def step(grid: Grid[Boolean]): Grid[Boolean] = {
    // TODO: use view for zipWithIndex?
    for ((row, y) <- grid.zipWithIndex)
      yield for ((cell, x) <- row.zipWithIndex)
        yield {
          val pos = Pos(x, y)
          val neighbors = Pos.axisOffsets.map(pos + _).filter(grid.containsPos).map(grid(_))
          val bugs = neighbors.count(identity)
          cell match {
            case true if bugs != 1 => false
            case false if bugs == 1 || bugs == 2 => true
            case c => c
          }
        }
  }

  def findCycleGrid(grid: Grid[Boolean]): Grid[Boolean] = {
    NaiveCycleFinder.find(grid, step).cycleHead
  }

  def biodiversityRating(grid: Grid[Boolean]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell
      i = y * row.size + x
    } yield 1 << i).sum
  }

  def findCycleBiodiversityRating(grid: Grid[Boolean]): Int = biodiversityRating(findCycleGrid(grid))

  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.map(_ == '#').toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findCycleBiodiversityRating(parseGrid(input)))
  }
}
