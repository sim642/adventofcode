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

  def resourceValue(grid: Grid[Char], after: Int = 10): Int = {
    val it = Iterator.iterate(grid)(step)
    val finalGrid = it.drop(after).head
    val wooded = finalGrid.countGrid(_ == '|')
    val lumberyards = finalGrid.countGrid(_ == '#')
    wooded * lumberyards
  }

  def resourceValue2(grid: Grid[Char], after: Int = 1000000000): Int = {

    val (mu, lambda) = FloydSolution.floyd(grid, step)
    println(mu)
    println(lambda)

    val finalAfter = (after - mu) % lambda
    println(finalAfter)
    resourceValue(grid, mu + finalAfter)
    /*val it = Iterator.iterate(grid)(step)
    for ((grid, i) <- it.zipWithIndex) {
      println(i)
      //printGrid(grid)
      val wooded = grid.countGrid(_ == '|')
      val lumberyards = grid.countGrid(_ == '#')
      println(s"$wooded $lumberyards ${wooded * lumberyards}")
      println()
    }
    ???*/
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
    //println(resourceValue(parseInput(input), after = 1000)) // 169106
    //println(resourceValue2(parseInput(input), after = 1000)) // 169106
    println(resourceValue2(parseInput(input)))

    // 653184 - too high
  }
}
