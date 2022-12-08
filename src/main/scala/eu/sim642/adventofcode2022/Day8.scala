package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day8 {

  def countVisibleTrees(grid: Grid[Int]): Int = {
    val left = grid.map(_.scanLeft(-1)(_ max _))
    val right = grid.map(_.scanRight(-1)(_ max _))
    val top = grid.transpose.map(_.scanLeft(-1)(_ max _))
    val bottom = grid.transpose.map(_.scanRight(-1)(_ max _))
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell > left(y)(x) || cell > right(y)(x + 1) || cell > top(x)(y) || cell > bottom(x)(y + 1)
    } yield ()).size
  }

  def scenicScore(grid: Grid[Int], gridTranspose: Grid[Int], pos: Pos): Int = {
    // TODO: clean up
    val cell = grid(pos)
    val left = grid(pos.y).lastIndexWhere(_ >= cell, pos.x - 1)
    val left2 = if (left < 0) then pos.x else pos.x - left
    val right = grid(pos.y).indexWhere(_ >= cell, pos.x + 1)
    val right2 = if (right < 0) then grid(pos.y).size - 1 - pos.x else right - pos.x
    val top = gridTranspose(pos.x).lastIndexWhere(_ >= cell, pos.y - 1)
    val top2 = if (top < 0) then pos.y else pos.y - top
    val bottom = gridTranspose(pos.x).indexWhere(_ >= cell, pos.y + 1)
    val bottom2 = if (bottom < 0) then grid.size - 1 - pos.y else bottom - pos.y
    left2 * right2 * top2 * bottom2
  }

  def maxScenicScore(grid: Grid[Int]): Int = {
    val gridTranspose = grid.transpose
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
    } yield scenicScore(grid, gridTranspose, Pos(x, y))).max
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countVisibleTrees(parseGrid(input)))
    println(maxScenicScore(parseGrid(input)))
  }
}
