package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day19.Grid
import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2018.Day2.HeadIterator
import eu.sim642.adventofcode2017.Day21.GridOps
import eu.sim642.adventofcode2017.Day6.FloydSolution
import eu.sim642.adventofcode2017.Day14.PosGrid
import eu.sim642.adventofcode2017.Day19.PosGrid2

object Day18 {

  implicit class GridOps2[A](grid: Grid[A]) {
    def slidingGrid(size: Int): Iterator[Iterator[Grid[A]]] = {
      grid.sliding(size).map({ rows =>
        rows.map(_.sliding(size).toVector).transpose.iterator
      })
    }
  }

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


  def parseInput(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day18.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(resourceValueIterate(parseInput(input)))
    println(resourceValueCycle(parseInput(input)))
  }
}
