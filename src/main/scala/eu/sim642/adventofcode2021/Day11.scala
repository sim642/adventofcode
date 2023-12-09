package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec
import scala.collection.{View, mutable}

object Day11 {

  def flashable(grid: Grid[Int]): View[Pos] = {
    for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell > 9
      pos = Pos(x, y)
    } yield pos
  }

  sealed trait Solution {
    def simulateStep(grid: Grid[Int]): (Grid[Int], Int)

    private def iterateFlashes(initialGrid: Grid[Int]): Iterator[Int] = {
      Iterator.unfold(initialGrid)(grid => {
        Some(simulateStep(grid).swap)
      })
    }

    def countFlashes(initialGrid: Grid[Int], steps: Int = 100): Int = {
      iterateFlashes(initialGrid).take(steps).sum
    }

    def findSimultaneousFlash(initialGrid: Grid[Int]): Int = {
      iterateFlashes(initialGrid).indexOf(initialGrid.sizeGrid) + 1
    }
  }

  /**
   * Solution, which naively and iteratively performs all flashes in a step until no more are possible.
   */
  object NaiveSolution extends Solution {
    def simulateStep(grid: Grid[Int]): (Grid[Int], Int) = {

      @tailrec
      def helper(grid: Grid[Int]): (Grid[Int], Int) = {
        val flashes = flashable(grid).toSeq

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

      @tailrec
      def dfs(todo: List[Pos], grid: Grid[Int], visited: Set[Pos]): (Grid[Int], Set[Pos]) = todo match {
        case Nil => (grid, visited)
        case pos :: todo if !visited.contains(pos) =>
          var grid2 = grid.updatedGrid(pos, 0)
          val newNeighbors = Pos.allOffsets.map(pos + _).filter(grid2.containsPos).filter(!visited.contains(_))
          val newGrid = newNeighbors.foldLeft(grid2)((grid, pos) => grid.updatedGrid(pos, grid(pos) + 1))
          val newTodo = newNeighbors.filter(newGrid(_) > 9) ++: todo
          val newVisited = visited + pos
          dfs(newTodo, newGrid, newVisited)
        case _ :: xs => dfs(xs, grid, visited)
      }

      val grid2 = grid.mapGrid(_ + 1)
      val (newGrid, visited) = dfs(flashable(grid2).toList, grid2, Set.empty)
      (newGrid, visited.size)
    }
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import DFSSolution._

    println(countFlashes(parseGrid(input)))
    println(findSimultaneousFlash(parseGrid(input)))
  }
}
