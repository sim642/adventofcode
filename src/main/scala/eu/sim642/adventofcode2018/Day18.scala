package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2018.Day2.HeadIterator
import eu.sim642.adventofcode2017.Day21.GridOps
import eu.sim642.adventofcode2017.Day6.FloydSolution

object Day18 {

  implicit class GridOps2[A](grid: Grid[A]) {
    def slidingGrid(size: Int): Iterator[Iterator[Grid[A]]] = {
      grid.sliding(size).map({ rows =>
        rows.map(_.sliding(size).toVector).transpose.toIterator
      })
    }
  }

  def step(grid: Grid[Char]): Grid[Char] = {
    val paddingRow = Vector.fill(grid(0).size + 2)('.')
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

    paddedGrid.slidingGrid(3).map(_.map(stepTile).toVector).toVector
  }

  def resourceValue(grid: Grid[Char]): Int = {
    val wooded = grid.countGrid(_ == '|')
    val lumberyards = grid.countGrid(_ == '#')
    wooded * lumberyards
  }

  def resourceValueIterate(grid: Grid[Char], after: Int = 10): Int = {
    val it = Iterator.iterate(grid)(step)
    val finalGrid = it.drop(after).head
    resourceValue(finalGrid)
  }

  def resourceValueCycle(grid: Grid[Char], after: Int = 1000000000): Int = {
    val (mu, lambda) = FloydSolution.floyd(grid, step)
    val afterMu = (after - mu) % lambda
    resourceValueIterate(grid, mu + afterMu)
  }

  def printGrid(grid: Grid[Char]): Unit = {
    for (row <- grid) {
      for (cell <- row)
        print(cell)
      println()
    }
  }


  def parseInput(input: String): Grid[Char] = input.lines.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(resourceValueIterate(parseInput(input)))
    println(resourceValueCycle(parseInput(input)))
  }
}
