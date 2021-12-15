package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{Dijkstra, GraphSearch, TargetNode}
import eu.sim642.adventofcodelib.pos.Pos

object Day15 {

  def lowestRiskPath(grid: Grid[Int]): Int = {

    val graphSearch = new GraphSearch[Pos] with TargetNode[Pos] {
      override val startNode: Pos = Pos.zero

      override def neighbors(pos: Pos): IterableOnce[(Pos, Int)] = {
        for {
          offset <- Pos.axisOffsets.iterator
          newPos = pos + offset
          if grid.containsPos(newPos)
        } yield newPos -> grid(newPos)
      }

      override val targetNode: Pos = Pos(grid(0).size - 1, grid.size - 1)
    }
    // A* with Manhattan distance heuristic is a bit slower (heuristic overhead?)

    Dijkstra.search(graphSearch).target.get._2
  }

  def extendGrid(grid: Grid[Int]): Grid[Int] = {
    Vector.tabulate(5, 5)(_ + _)
      .mapGrid(i => grid.mapGrid(x => (x + i - 1) % 9 + 1))
      .flattenGrid
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(lowestRiskPath(parseGrid(input)))
    println(lowestRiskPath(extendGrid(parseGrid(input))))
  }
}
