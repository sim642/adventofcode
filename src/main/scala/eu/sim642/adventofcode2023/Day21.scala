package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{GraphSearch, SimultaneousBFS, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._

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

  // copied from 2021 day 25
  extension (pos: Pos) {
    def %+(other: Pos): Pos = Pos(pos.x %+ other.x, pos.y %+ other.y)
  }

  def countReachableExactlyInfinite(grid: Grid[Char], steps: Int = 26501365): Int = {
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

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day21.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countReachableExactly(parseGrid(input)))
    println(countReachableExactlyInfinite(parseGrid(input)))
  }
}
