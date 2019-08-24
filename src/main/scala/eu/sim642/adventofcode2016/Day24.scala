package eu.sim642.adventofcode2016

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._

object Day24 {

  def findPois(grid: Grid[Char]): Map[Int, Pos] = {
    (for {
      (row, y) <- grid.zipWithIndex.iterator
      (cell, x) <- row.zipWithIndex.iterator
      if cell.isDigit
    } yield cell.asDigit -> Pos(x, y)).toMap
  }

  def getPoiDistMatrix(grid: Grid[Char], pois: Map[Int, Pos]): Map[Int, Map[Int, Int]] = {
    // transform instead of mapValues because mapValues is lazy and recomputes everything...
    pois.transform({ (_, fromPos) =>

      val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
        override val startNode: Pos = fromPos

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
          for {
            offset <- Pos.axisOffsets
            newPos = pos + offset
            if grid(newPos) != '#'
          } yield newPos
        }
      }

      val distances = BFS.traverse(graphTraversal).distances
      pois.transform((_, toPos) => distances(toPos))
    })
  }

  def tsp[A](distMatrix: Map[A, Map[A, Int]], start: A): Int = {
    (distMatrix.keySet - start).toVector
      .permutations
      .map({ path =>
        distMatrix(start)(path.head) +
          path.zip(path.tail)
            .map({ case (from, to) => distMatrix(from)(to) })
            .sum
      }).min
  }

  def shortestRoute(grid: Grid[Char]): Int = {
    val pois = findPois(grid)
    val distMatrix = getPoiDistMatrix(grid, pois)
    tsp(distMatrix, 0)
  }

  def tspReturn[A](distMatrix: Map[A, Map[A, Int]], start: A): Int = {
    (distMatrix.keySet - start).toVector
      .permutations
      .map({ path =>
        distMatrix(start)(path.head) +
          path.zip(path.tail)
            .map({ case (from, to) => distMatrix(from)(to) })
            .sum +
          distMatrix(path.last)(start)
      }).min
  }

  def shortestRouteReturn(grid: Grid[Char]): Int = {
    val pois = findPois(grid)
    val distMatrix = getPoiDistMatrix(grid, pois)
    tspReturn(distMatrix, 0)
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def shortestRoute(input: String): Int = shortestRoute(parseGrid(input))

  def shortestRouteReturn(input: String): Int = shortestRouteReturn(parseGrid(input))

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day24.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(shortestRoute(input))
    println(shortestRouteReturn(input))
  }
}