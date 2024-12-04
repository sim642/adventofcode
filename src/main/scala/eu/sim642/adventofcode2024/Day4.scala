package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day4 {

  def countXMAS(grid: Grid[Char]): Int = {
    def countHorizontal(grid: Grid[Char]): Int = {
      grid
        .map(_.sliding(4).count(_ == "XMAS".toSeq))
        .sum
    }

    def countDiagonal(grid: Grid[Char]): Int = {
      grid
        .slidingGrid(4)
        .flatten
        .count(w => w(0)(0) == 'X' && w(1)(1) == 'M' && w(2)(2) == 'A' && w(3)(3) == 'S')
    }

    val grids1 = Seq(
      grid,
      grid.map(_.reverse),
      grid.transpose,
      grid.transpose.map(_.reverse)
    )

    val grids2 = Seq(
      grid,
      grid.map(_.reverse),
      grid.reverse,
      grid.reverse.map(_.reverse)
    )

    grids1.map(countHorizontal).sum + grids2.map(countDiagonal).sum
  }

  def countCrossMAS(grid: Grid[Char]): Int = {
    def countCross(grid: Grid[Char]): Int = {
      grid
        .slidingGrid(3)
        .flatten
        .count(w => w(0)(0) == 'M' && w(0)(2) == 'S' && w(1)(1) == 'A' && w(2)(0) == 'M' && w(2)(2) == 'S')
    }

    val grids = Seq(
      grid,
      grid.map(_.reverse),
      grid.transpose,
      grid.transpose.map(_.reverse)
    )

    grids.map(countCross).sum
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day4.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countXMAS(parseGrid(input)))
    println(countCrossMAS(parseGrid(input)))
  }
}
