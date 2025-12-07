package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day7 {

  private val cellOffsets = Map(
    'S' -> Seq(Pos(0, 1)),
    '.' -> Seq(Pos(0, 1)),
    '^' -> Seq(Pos(-1, 0), Pos(1, 0)),
  )

  def countBeamSplits(grid: Grid[Char]): Int = {
    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = grid.posOf('S')

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        cellOffsets(grid(pos)).map(pos + _).filter(grid.containsPos)
      }
    }

    BFS.traverse(graphTraversal)
      .nodes
      .count(pos => grid(pos) == '^')
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBeamSplits(parseGrid(input)))
  }
}
