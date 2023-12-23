package eu.sim642.adventofcode2023

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*

object Day23 {

  case class HikePos(pos: Pos, path: Set[Pos])

  private val slopeOffsets = Map(
    '^' -> Pos(0, -1),
    '>' -> Pos(1, 0),
    'v' -> Pos(0, 1),
    '<' -> Pos(-1, 0),
  )

  def longestHike(grid: Grid[Char]): Int = {

    val graphTraversal = new GraphTraversal[HikePos] with UnitNeighbors[HikePos] {
      override val startNode: HikePos = HikePos(Pos(1, 0), Set(Pos(1, 0))) // TODO: don't hardcode

      override def unitNeighbors(hikePos: HikePos): IterableOnce[HikePos] = {
        val HikePos(pos, path) = hikePos
        for {
          offset <- if (grid(pos) == '.') Pos.axisOffsets else Seq(slopeOffsets(grid(pos)))
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) != '#'
          if !path.contains(newPos)
        } yield HikePos(newPos, path + newPos)
      }
    }

    val targetPos = Pos(grid(0).size - 2, grid.size - 1)

    val asd = BFS.traverse(graphTraversal).distances
      .filter(_._1.pos == targetPos)

    asd.values.toSet.foreach(println)
    asd
      .values
      .max
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day23.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(longestHike(parseGrid(input)))
  }
}
