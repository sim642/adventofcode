package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day12 {

  def height(c: Char): Char = c match {
    case 'S' => 'a'
    case 'E' => 'z'
    case c => c
  }

  trait Part {
    def isTargetPos(grid: Grid[Char], pos: Pos): Boolean

    def fewestStepsToBestSignal(grid: Grid[Char]): Int = {

      val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {

        override val startNode: Pos = grid.posOf('E')

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
          for {
            offset <- Pos.axisOffsets.iterator
            newPos = pos + offset
            if grid.containsPos(newPos)
            if height(grid(pos)) <= height(grid(newPos)) + 1
          } yield newPos
        }

        override def isTargetNode(pos: Pos, dist: Int): Boolean = isTargetPos(grid, pos)
      }

      BFS.search(graphSearch).target.get._2
    }
  }

  object Part1 extends Part {
    override def isTargetPos(grid: Grid[Char], pos: Pos): Boolean = grid(pos) == 'S'
  }

  object Part2 extends Part {
    override def isTargetPos(grid: Grid[Char], pos: Pos): Boolean = height(grid(pos)) == 'a'
  }


  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.fewestStepsToBestSignal(parseGrid(input)))
    println(Part2.fewestStepsToBestSignal(parseGrid(input)))
  }
}
