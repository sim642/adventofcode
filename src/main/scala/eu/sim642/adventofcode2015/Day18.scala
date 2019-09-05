package eu.sim642.adventofcode2015

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day18 {

  // copied & modified from 2018 Day 18

  def step(grid: Grid[Boolean]): Grid[Boolean] = {
    for ((row, y) <- grid.zipWithIndex)
      yield for ((cell, x) <- row.zipWithIndex)
        yield {
          val pos = Pos(x, y)
          val neighborsOn = Pos.allOffsets.map(pos + _).filter(grid.containsPos).count(grid(_))
          (cell, neighborsOn) match {
            case (true, 2 | 3) => true
            case (true, _) => false
            case (false, 3) => true
            case (false, _) => false
          }
        }
  }

  def countOnIterate(grid: Grid[Boolean], after: Int): Int = {
    val it = Iterator.iterate(grid)(step)
    val finalGrid = it(after)
    finalGrid.countGrid(identity)
  }

  def countOnIterate(input: String, after: Int = 100): Int = countOnIterate(parseGrid(input), after)

  def printGrid(grid: Grid[Boolean]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(if (cell) '#' else '.')
      println()
    }
  }


  def parseGrid(input: String): Grid[Boolean] = input.linesIterator.map(_.toVector).toVector.mapGrid({
    case '#' => true
    case '.' => false
  })


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countOnIterate(input))
  }
}
