package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.cycle.{NaiveCycleFinder, NaiverCycleFinder}

object Day14 {

  def rollLeft(row: Vector[Char]): Vector[Char] = {

    def rollOne(row: List[Char]): List[Char] = row match {
      case Nil => Nil
      case _ :: Nil => row
      case '.' :: 'O' :: newColumn => 'O' :: rollOne('.' :: newColumn)
      case prev :: newColumn => prev :: rollOne(newColumn)
    }

    NaiveCycleFinder.find(row.toList, rollOne).cycleHead.toVector // TODO: simpler fixpoint finding?
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
