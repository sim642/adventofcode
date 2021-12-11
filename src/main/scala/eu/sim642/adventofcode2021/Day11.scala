package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec
import scala.collection.mutable

object Day11 {

  sealed trait Solution {
    def simulateStep(grid: Grid[Int]): (Grid[Int], Int)

    def countFlashes(initialGrid: Grid[Int], steps: Int): Int = {
      Iterator.unfold(initialGrid)(grid => {
        Some(simulateStep(grid).swap)
      }).take(steps).sum
    }

    def findSimultaneousFlash(initialGrid: Grid[Int]): Int = {
      val gridSize = initialGrid.size * initialGrid(0).size
      Iterator.unfold(initialGrid)(grid => {
        Some(simulateStep(grid).swap)
      }).indexOf(gridSize) + 1
    }
  }

  /**
   * Solution, which naively and iteratively performs all flashes in a step until no more are possible.
   */
  object NaiveSolution extends Solution {
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
  }

  /**
   * Solution, which performs flashes in a single pass using grid-changing DFS on the flashing octopi.
   */
  object DFSSolution extends Solution {
    def simulateStep(grid: Grid[Int]): (Grid[Int], Int) = {
      // TODO: clean up
      val grid2 = grid.mapGrid(_ + 1)
      val visited = mutable.Set.empty[Pos]
      var newGrid = grid2

      @tailrec
      def dfs(todo: List[Pos]): Unit = todo match {
        case Nil =>
        case x :: xs if !visited.contains(x) =>
          visited += x
          newGrid = newGrid.updatedGrid(x, 0)
          var newTodo = xs
          for {
            newPos <- Pos.allOffsets.map(x + _).filter(newGrid.containsPos)
            if !visited.contains(newPos)
          } {
            val newValue = newGrid(newPos) + 1
            newGrid = newGrid.updatedGrid(newPos, newValue)
            if (newValue > 9)
              newTodo = newPos :: newTodo
          }
          dfs(newTodo)
        case _ :: xs => dfs(xs)
      }

      val flashes = (for {
        (row, y) <- grid2.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell > 9
        pos = Pos(x, y)
      } yield pos).toList

      dfs(flashes)

      (newGrid, visited.size)
    }
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import DFSSolution._

    println(countFlashes(parseGrid(input), 100))
    println(findSimultaneousFlash(parseGrid(input)))
  }
}
