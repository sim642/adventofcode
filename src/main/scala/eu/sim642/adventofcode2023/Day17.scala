package eu.sim642.adventofcode2023

import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day17 {

  case class Crucible(pos: Pos, direction: Pos, directionCount: Int)

  def leastHeatLoss(grid: Grid[Int]): Int = {

    val graphSearch = new GraphSearch[Crucible] {
      override val startNode: Crucible = Crucible(Pos.zero, Pos.zero, 0)

      override def neighbors(crucible: Crucible): IterableOnce[(Crucible, Int)] = {
        val Crucible(pos, direction, directionCount) = crucible
        for {
          offset <- Pos.axisOffsets
          if offset != -direction
          if !(directionCount == 3 && offset == direction)
          newPos = pos + offset
          if grid.containsPos(newPos)
          newDirectionCount = if (offset == direction) directionCount + 1 else 1
        } yield Crucible(newPos, offset, newDirectionCount) -> grid(newPos)
      }

      private val targetPos = Pos(grid(0).size - 1, grid.size - 1)

      override def isTargetNode(crucible: Crucible, dist: Int): Boolean = crucible.pos == targetPos
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  // TODO: deduplicate
  def leastHeatLossUltra(grid: Grid[Int]): Int = {

    val graphSearch = new GraphSearch[Crucible] {
      override val startNode: Crucible = Crucible(Pos.zero, Pos.zero, 0)

      override def neighbors(crucible: Crucible): IterableOnce[(Crucible, Int)] = {
        val Crucible(pos, direction, directionCount) = crucible
        for {
          offset <- Pos.axisOffsets
          if offset != -direction
          if direction == Pos.zero || (offset == direction && directionCount < 10) || (offset != direction && directionCount >= 4)
          newPos = pos + offset
          if grid.containsPos(newPos)
          newDirectionCount = if (offset == direction) directionCount + 1 else 1
        } yield Crucible(newPos, offset, newDirectionCount) -> grid(newPos)
      }

      private val targetPos = Pos(grid(0).size - 1, grid.size - 1)

      override def isTargetNode(crucible: Crucible, dist: Int): Boolean = crucible.pos == targetPos && crucible.directionCount >= 4
    }

    Dijkstra.search(graphSearch).target.get._2
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.map(_.asDigit).toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(leastHeatLoss(parseGrid(input)))
    println(leastHeatLossUltra(parseGrid(input)))
  }
}
