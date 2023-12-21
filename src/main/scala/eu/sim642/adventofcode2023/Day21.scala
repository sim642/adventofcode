package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{GraphSearch, SimultaneousBFS, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._
import Integral.Implicits._

object Day21 {

  def countReachableExactly(grid: Grid[Char], steps: Int = 64): Int = {

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = grid.posOf('S')

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) != '#'
        } yield newPos
      }

      override def isTargetNode(pos: Pos, dist: Int): Boolean = dist == steps
    }

    SimultaneousBFS.search(graphSearch).distances.count(_._2 % 2 == steps % 2)
  }

  trait Part2Solution {
    def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long
  }

  object NaivePart2Solution extends Part2Solution {

    // copied from 2021 day 25
    extension (pos: Pos) {
      def %+(other: Pos): Pos = Pos(pos.x %+ other.x, pos.y %+ other.y)
    }

    override def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long = {
      val gridSize = Pos(grid(0).size, grid.size)

      val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {
        override val startNode: Pos = grid.posOf('S')

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
          for {
            offset <- Pos.axisOffsets
            newPos = pos + offset
            if grid(newPos %+ gridSize) != '#'
          } yield newPos
        }

        override def isTargetNode(pos: Pos, dist: Int): Boolean = dist == steps
      }

      SimultaneousBFS.search(graphSearch).distances.count(_._2 % 2 == steps % 2)
    }
  }

  object QuadraticPart2Solution extends Part2Solution {

    override def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long = {
      val (q, r) = steps /% 131
      //println((q, r))

      val x1 = 0
      val y1 = NaivePart2Solution.countReachableExactlyInfinite(grid, r)
      val x2 = 1
      val y2 = NaivePart2Solution.countReachableExactlyInfinite(grid, r + 131)
      val x3 = 2
      val y3 = NaivePart2Solution.countReachableExactlyInfinite(grid, r + 2 * 131)

      def f(x: Long): Long = {
        y1 * (x - x2) * (x - x3) / (x1 - x2) / (x1 - x3) +
          y2 * (x - x1) * (x - x3) / (x2 - x1) / (x2 - x3) +
          y3 * (x - x1) * (x - x2) / (x3 - x1) / (x3 - x2)
      }

      f(q)
    }
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import QuadraticPart2Solution._

    println(countReachableExactly(parseGrid(input)))
    println(countReachableExactlyInfinite(parseGrid(input)))
  }
}
