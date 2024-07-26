package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.IntegralImplicits.*
import eu.sim642.adventofcodelib.graph.{GraphSearch, SimultaneousBFS, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.{Grid, Polynomial}

import scala.math.Integral.Implicits.*

object Day21 {

  def gardenSearch(startPos: Pos, grid: Pos => Boolean, maxDist: Int): GraphSearch[Pos] & UnitNeighbors[Pos] = {
    new GraphSearch[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = startPos

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid(newPos)
        } yield newPos
      }

      override def isTargetNode(pos: Pos, dist: Int): Boolean = dist == maxDist
    }
  }

  def countReachableExactly(grid: Grid[Char], steps: Int = 64): Int = {
    val graphSearch = gardenSearch(grid.posOf('S'), pos => grid.containsPos(pos) && grid(pos) != '#', steps)
    SimultaneousBFS.search(graphSearch).distances.count(_._2 % 2 == steps % 2)
  }

  trait Part2Solution {
    def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long
  }

  // copied from 2021 day 25
  extension (pos: Pos) {
    def %+(other: Pos): Pos = Pos(pos.x %+ other.x, pos.y %+ other.y)
  }

  object NaivePart2Solution extends Part2Solution {
    override def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long = {
      val gridSize = Pos(grid(0).size, grid.size)
      val graphSearch = gardenSearch(grid.posOf('S'), pos => grid(pos %+ gridSize) != '#', steps)
      SimultaneousBFS.search(graphSearch).distances.count(_._2 % 2 == steps % 2)
    }
  }

  object QuadraticPart2Solution extends Part2Solution {
    override def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Long = {
      val gridSize = Pos(grid(0).size, grid.size)
      assert(gridSize.x == 131 && gridSize.y == 131)

      val (q, r) = steps /% 131
      //assert(r == 65) // TODO: need this?

      def maxDist(x: Int) = r + x * 131

      val graphSearch = gardenSearch(grid.posOf('S'), pos => grid(pos %+ gridSize) != '#', maxDist(2))
      val distances = SimultaneousBFS.search(graphSearch).distances

      def p(x: Int): Pos = Pos(x, distances.count({ case (_, dist) =>
        val steps = maxDist(x)
        dist <= steps && dist % 2 == steps % 2
      }))

      Polynomial.fitQuadratic(p(0), p(1), p(2))(q)
    }
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import QuadraticPart2Solution.*

    println(countReachableExactly(parseGrid(input)))
    println(countReachableExactlyInfinite(parseGrid(input)))
  }
}
