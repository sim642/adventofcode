package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day11 {

  def simulateStep(grid: Grid[Int]): (Grid[Int], Int) = {

    @tailrec
    def helper(grid: Grid[Int]): (Grid[Int], Int) = {
      val flashes = (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell > 9
        pos = Pos(x, y)
      } yield pos).toSeq

      if (flashes.isEmpty) {
        val newGrid = grid.mapGrid({
          case level if level < 0 => 0
          case level => level
        })
        val flashCount = grid.countGrid(_ < 0)
        (newGrid, flashCount)
      }
      else {
        val flashNeighbors = (for {
          flash <- flashes.view
          neighbor <- Pos.allOffsets.map(flash + _).filter(grid.containsPos)
        } yield neighbor).toSeq

        val grid2 = flashes.foldLeft(grid)((grid, flash) => grid.updatedGrid(flash, -10000))
        val grid3 = flashNeighbors.foldLeft(grid2)((grid, neighbor) => grid.updatedGrid(neighbor, grid(neighbor) + 1))
        helper(grid3)
      }
    }

    helper(grid.mapGrid(_ + 1))
  }

  def countFlashes(initialGrid: Grid[Int], steps: Int): Int = {
    Iterator.unfold(initialGrid)(grid => {
      Some(simulateStep(grid).swap)
    }).take(steps).sum
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countFlashes(parseGrid(input), 100))
  }
}
