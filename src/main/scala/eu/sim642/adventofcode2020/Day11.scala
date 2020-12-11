package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.pos.Pos

object Day11 {

  sealed trait Part {
    protected def getNeighborsGrid(grid: Grid[Char]): Grid[Seq[Pos]]

    protected val tolerance: Int

    // based on 2018 Day 18
    private def step(grid: Grid[Char], neighborsGrid: Grid[Seq[Pos]]): Grid[Char] = {
      for ((row, y) <- grid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            // TODO: avoid these on floors
            val pos = Pos(x, y)
            val neighbors = neighborsGrid(pos).map(grid(_))
            val occupied = neighbors.count(_ == '#')
            cell match {
              case 'L' if occupied == 0 => '#'
              case '#' if occupied >= tolerance => 'L'
              case c => c
            }
          }
    }

    def countOccupiedStable(grid: Grid[Char]): Int = {
      val neighborsGrid = getNeighborsGrid(grid)
      val cycle = NaiveCycleFinder.find(grid, step(_, neighborsGrid))
      assert(cycle.cycleLength == 1)
      cycle.cycleHead.countGrid(_ == '#')
    }
  }

  object Part1 extends Part {

    override protected def getNeighborsGrid(grid: Grid[Char]): Grid[Seq[Pos]] = {
      for ((row, y) <- grid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            val pos = Pos(x, y)
            Pos.allOffsets.map(pos + _).filter(grid.containsPos)
          }
    }

    override protected val tolerance: Int = 4
  }

  object Part2 extends Part {

    override protected def getNeighborsGrid(grid: Grid[Char]): Grid[Seq[Pos]] = {

      def findVisible(pos: Pos, offset: Pos): Option[Pos] = {
        Iterator.iterate(pos)(_ + offset)
          .drop(1)
          .takeWhile(grid.containsPos)
          .find(grid(_) != '.')
      }

      for ((row, y) <- grid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            val pos = Pos(x, y)
            Pos.allOffsets.flatMap(findVisible(pos, _))
          }
    }

    override protected val tolerance: Int = 5
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countOccupiedStable(parseGrid(input)))
    println(Part2.countOccupiedStable(parseGrid(input)))
  }
}
