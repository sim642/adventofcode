package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.cycle.NaiverCycleFinder

object Day14 {

  def rollLeft(row: Vector[Char]): Vector[Char] = {

    def helper(empty: Int, row: List[Char]): List[Char] = row match {
      case '.' :: newRow => helper(empty + 1, newRow) // remember . for following O
      case 'O' :: newRow => 'O' :: helper(empty, newRow) // put O at first remembered .
      case '#' :: newRow => List.fill(empty)('.') ::: '#' :: helper(0, newRow) // materialize .-s and start over
      case Nil => List.fill(empty)('.') // materialize .-s at the end
      case _ :: _ => throw new IllegalArgumentException("illegal cell")
    }

    helper(0, row.toList).toVector
  }

  def rollNorth(grid: Grid[Char]): Grid[Char] = grid.transpose.map(rollLeft).transpose
  def rollWest(grid: Grid[Char]): Grid[Char] = grid.map(rollLeft)
  def rollSouth(grid: Grid[Char]): Grid[Char] = rollNorth(grid.reverse).reverse
  def rollEast(grid: Grid[Char]): Grid[Char] = rollWest(grid.map(_.reverse)).map(_.reverse)
  def rollCycle(grid: Grid[Char]): Grid[Char] = rollEast(rollSouth(rollWest(rollNorth(grid))))

  trait Part {
    def roll(grid: Grid[Char]): Grid[Char]

    def totalNorthLoad(grid: Grid[Char]): Int = {
      roll(grid)
        .transpose
        .map(_.reverse.zipWithIndex.map({
          case ('O', i) => i + 1
          case _ => 0
        }).sum)
        .sum
    }
  }

  object Part1 extends Part {
    override def roll(grid: Grid[Char]): Grid[Char] = rollNorth(grid)
  }

  object Part2 extends Part {
    override def roll(grid: Grid[Char]): Grid[Char] =
      NaiverCycleFinder.find(grid, rollCycle)(1000000000)
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.totalNorthLoad(parseGrid(input)))
    println(Part2.totalNorthLoad(parseGrid(input)))
  }
}
