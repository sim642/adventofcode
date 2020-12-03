package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day3 {

  def countSlopeTrees(grid: Grid[Boolean], slope: Pos = Pos(3, 1)): Int = {
    def gridRepeat(pos: Pos): Boolean = {
      val row = grid(pos.y)
      row(pos.x % row.size)
    }

    // original row-by-row solution generalized to bigger y steps
    (grid.indices by slope.y).view.zipWithIndex.count({ case (y, i) =>
      val x = slope.x * i
      gridRepeat(Pos(x, y))
    })

    // alternative iterative solution
    /*@tailrec
    def helper(pos: Pos, count: Int): Int = {
      if (pos.y < grid.size) {
        val countDelta = if (gridRepeat(pos)) 1 else 0
        helper(pos + slope, count + countDelta)
      } else
        count
    }

    helper(Pos.zero, 0)*/
  }

  private val multiplySlopes = Seq(
    Pos(1, 1),
    Pos(3, 1),
    Pos(5, 1),
    Pos(7, 1),
    Pos(1, 2),
  )

  def multiplySlopeTrees(grid: Grid[Boolean]): Long = {
    multiplySlopes.map(countSlopeTrees(grid, _)).map(_.toLong).product
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid({
    case '#' => true
    case '.' => false
  })

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countSlopeTrees(parseGrid(input)))
    println(multiplySlopeTrees(parseGrid(input)))

    // part 2: 712691360 - too low
  }
}
