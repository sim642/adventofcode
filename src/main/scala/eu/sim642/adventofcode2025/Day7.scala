package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

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

  def countTimelines(grid: Grid[Char]): Long = {

    @tailrec
    def helper(y: Int, xs: Map[Int, Long]): Long = {
      if (y >= grid.size)
        xs.values.sum
      else {
        val newXs =
          (for {
            (x, count) <- xs.view
            pos = Pos(x, y)
            offset <- cellOffsets(grid(pos))
            newPos = pos + offset
          } yield newPos.x -> count).groupMapReduce(_._1)(_._2)(_ + _)

        helper(y + 1, newXs)
      }
    }

    helper(0, Map(grid.posOf('S').x -> 1))
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day7.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countBeamSplits(parseGrid(input)))
    println(countTimelines(parseGrid(input)))

    // part 2: 2012790981 (Int overflowed in countTimelines)
  }
}
