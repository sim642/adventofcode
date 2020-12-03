package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid

object Day3 {

  def countSlopeTrees(grid: Grid[Char], slope: Int = 3): Int = {
    grid.zipWithIndex.count({ case (row, y) =>
      val x = slope * y
      row(x % row.size) == '#'
    })
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countSlopeTrees(parseGrid(input)))
  }
}
