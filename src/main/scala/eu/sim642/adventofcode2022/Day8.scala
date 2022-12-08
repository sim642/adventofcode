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


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countVisibleTrees(parseGrid(input)))
  }
}
