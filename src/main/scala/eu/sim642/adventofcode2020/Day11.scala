package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.cycle.NaiveCycleFinder
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day11 {

  sealed trait Part {
    def step(grid: Grid[Char]): Grid[Char]

    def countOccupiedStable(grid: Grid[Char]): Int = {
      val cycle = NaiveCycleFinder.find(grid, step)
      assert(cycle.cycleLength == 1)
      cycle.cycleHead.countGrid(_ == '#')
    }
  }

  // TODO: reduce duplication

  object Part1 extends Part {
    // based on 2018 Day 18
    override def step(grid: Grid[Char]): Grid[Char] = {
      for ((row, y) <- grid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            val pos = Pos(x, y)
            val neighbors = Pos.allOffsets.map(pos + _).filter(grid.containsPos).map(grid(_))
            val occupied = neighbors.count(_ == '#')
            cell match {
              case 'L' if occupied == 0 => '#'
              case '#' if occupied >= 4 => 'L'
              case c => c
            }
          }
    }
  }

  object Part2 extends Part {
    // based on 2018 Day 18
    override def step(grid: Grid[Char]): Grid[Char] = {

      // TODO: precalculate/memoize findVisible
      @tailrec
      def findVisible(pos: Pos, offset: Pos): Option[Pos] = {
        val newPos = pos + offset
        if (grid.containsPos(newPos)) {
          grid(newPos) match {
            case 'L' | '#' => Some(newPos)
            case _ => findVisible(newPos, offset)
          }
        }
        else
          None
      }

      for ((row, y) <- grid.zipWithIndex)
        yield for ((cell, x) <- row.zipWithIndex)
          yield {
            val pos = Pos(x, y)
            val neighbors = Pos.allOffsets.flatMap(findVisible(pos, _)).map(grid(_))
            val occupied = neighbors.count(_ == '#')
            cell match {
              case 'L' if occupied == 0 => '#'
              case '#' if occupied >= 5 => 'L'
              case c => c
            }
          }
    }
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countOccupiedStable(parseGrid(input)))
    println(Part2.countOccupiedStable(parseGrid(input)))
  }
}
