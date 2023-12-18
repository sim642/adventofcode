package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.*
import eu.sim642.adventofcodelib.pos.Pos

object Day17 {

  case class Crucible(pos: Pos, direction: Pos, directionCount: Int)

  trait Part {
    protected def canMove(crucible: Crucible, offset: Pos): Boolean
    protected def canStop(crucible: Crucible): Boolean = true

    def leastHeatLoss(grid: Grid[Int]): Int = {

      val graphSearch = new GraphSearch[Crucible] {
        override val startNode: Crucible = Crucible(Pos.zero, Pos.zero, 0)

        override def neighbors(crucible: Crucible): IterableOnce[(Crucible, Int)] = {
          val Crucible(pos, direction, directionCount) = crucible
          for {
            offset <- Pos.axisOffsets
            if offset != -direction
            if canMove(crucible, offset)
            newPos = pos + offset
            if grid.containsPos(newPos)
            newDirectionCount = if (offset == direction) directionCount + 1 else 1
          } yield Crucible(newPos, offset, newDirectionCount) -> grid(newPos)
        }

        private val targetPos = Pos(grid(0).size - 1, grid.size - 1)

        override def isTargetNode(crucible: Crucible, dist: Int): Boolean = crucible.pos == targetPos && canStop(crucible)
      }

      Dijkstra.search(graphSearch).target.get._2
    }
  }

  object Part1 extends Part {
    override protected def canMove(crucible: Crucible, offset: Pos): Boolean =
      crucible.directionCount < 3 || offset != crucible.direction
  }

  object Part2 extends Part {
    override protected def canMove(crucible: Crucible, offset: Pos): Boolean = {
      val Crucible(pos, direction, directionCount) = crucible
      direction == Pos.zero || (offset == direction && directionCount < 10) || (offset != direction && directionCount >= 4)
    }

    override protected def canStop(crucible: Crucible): Boolean = crucible.directionCount >= 4
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.map(_.asDigit).toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.leastHeatLoss(parseGrid(input)))
    println(Part2.leastHeatLoss(parseGrid(input)))
  }
}
