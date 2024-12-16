package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch, TargetNode}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcode2018.Day13.DirectionPos

object Day16 {

  case class Reindeer(pos: Pos, direction: Pos = Pos(1, 0))

  def lowestScore(grid: Grid[Char]): Int = {

    val graphSearch = new GraphSearch[Reindeer] {
      override val startNode: Reindeer = Reindeer(grid.posOf('S'))

      override def neighbors(reindeer: Reindeer): IterableOnce[(Reindeer, Int)] = {
        Seq(
          reindeer.copy(pos = reindeer.pos + reindeer.direction) -> 1,
          reindeer.copy(direction = reindeer.direction.left) -> 1000,
          reindeer.copy(direction = reindeer.direction.right) -> 1000,
        )
          .filter(reindeer => grid(reindeer._1.pos) != '#')
      }

      private val targetPos: Pos = grid.posOf('E')

      override def isTargetNode(reindeer: Reindeer, dist: Int): Boolean = reindeer.pos == targetPos
    }

    Dijkstra.search(graphSearch).target.get._2
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(lowestScore(parseGrid(input)))
  }
}
