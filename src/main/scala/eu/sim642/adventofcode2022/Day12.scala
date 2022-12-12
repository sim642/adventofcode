package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day12 {

  def height(c: Char): Char = c match {
    case 'S' => 'a'
    case 'E' => 'z'
    case c => c
  }

  def fewestStepsToBestSignal(grid: Grid[Char]): Int = {

    val graphSearch = new GraphSearch[Pos] with UnitNeighbors[Pos] {

      override val startNode: Pos = grid.posOf('S')

      override def unitNeighbors(node: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets.iterator
          newPos = node + offset
          if grid.containsPos(newPos)
          if height(grid(newPos)) <= height(grid(node)) + 1
        } yield newPos
      }

      override def isTargetNode(node: Pos, dist: Int): Boolean = grid(node) == 'E'
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(fewestStepsToBestSignal(parseGrid(input)))
  }
}
