package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.cycle.NaiverCycleFinder

object Day18 {

  def step(grid: Grid[Char]): Grid[Char] = {
    /*val paddingRow = Vector.fill(grid(0).size + 2)('.')
    val paddedGrid: Grid[Char] = paddingRow +: grid.map('.' +: _ :+ '.') :+ paddingRow

    def stepTile(grid: Grid[Char]): Char = {
      val neighbors = (grid(0) ++ Vector(grid(1)(0), grid(1)(2)) ++ grid(2)).groupBy(c => c).mapValues(_.length).withDefaultValue(0)
      grid(1)(1) match {
        case '.' if neighbors('|') >= 3 => '|'
        case '|' if neighbors('#') >= 3 => '#'
        case '#' if neighbors('|') >= 1 && neighbors('#') >= 1 => '#'
        case '#' => '.'
        case c => c
      }
    }

    paddedGrid.slidingGrid(3).map(_.map(stepTile).toVector).toVector*/

    // TODO: use view for zipWithIndex?
    for ((row, y) <- grid.zipWithIndex)
      yield for ((cell, x) <- row.zipWithIndex)
        yield {
          val pos = Pos(x, y)
          val neighbors = Pos.allOffsets.map(pos + _).filter(grid.containsPos).map(grid(_))
          val trees = neighbors.count(_ == '|')
          val lumberyards = neighbors.count(_ == '#')
          cell match {
            case '.' if trees >= 3 => '|'
            case '|' if lumberyards >= 3 => '#'
            case '#' if trees >= 1 && lumberyards >= 1 => '#'
            case '#' => '.'
            case c => c
          }
        }
  }

  def resourceValue(grid: Grid[Char]): Int = {
    val trees = grid.countGrid(_ == '|')
    val lumberyards = grid.countGrid(_ == '#')
    trees * lumberyards
  }

  def resourceValueIterate(grid: Grid[Char], after: Int = 10): Int = {
    val it = Iterator.iterate(grid)(step)
    val finalGrid = it(after)
    resourceValue(finalGrid)
  }

  def resourceValueCycle(grid: Grid[Char], after: Int = 1000000000): Int = {
    resourceValue(NaiverCycleFinder.find(grid, step)(after))
  }

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }


  def parseInput(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(resourceValueIterate(parseInput(input)))
    println(resourceValueCycle(parseInput(input)))
  }
}
