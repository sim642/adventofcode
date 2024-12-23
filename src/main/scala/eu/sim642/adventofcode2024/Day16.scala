package eu.sim642.adventofcode2024

import eu.sim642.adventofcode2018.Day13.DirectionPos
import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.*
import eu.sim642.adventofcodelib.pos.Pos

object Day16 {

  case class Reindeer(pos: Pos, direction: Pos = Pos(1, 0))

  def forwardGraphSearch(grid: Grid[Char]): GraphSearch[Reindeer] = {
    new GraphSearch[Reindeer] {
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
  }

  def lowestScore(grid: Grid[Char]): Int = {
    val graphSearch = forwardGraphSearch(grid)
    Dijkstra.search(graphSearch).target.get._2
  }

  trait Part2Solution {
    def bestPathTiles(grid: Grid[Char]): Int
  }

  object BackwardNeighborsPart2Solution extends Part2Solution {
    override def bestPathTiles(grid: Grid[Char]): Int = {
      val forwardSearch = forwardGraphSearch(grid)
      val forwardResult = Dijkstra.search(forwardSearch)

      val backwardTraversal = new GraphTraversal[Reindeer] with UnitNeighbors[Reindeer] {
        override val startNode: Reindeer = forwardResult.target.get._1 // TODO: other orientations

        override def unitNeighbors(reindeer: Reindeer): IterableOnce[Reindeer] = {
          val distance = forwardResult.distances(reindeer)
          for {
            (oldReindeer, step) <- Seq(
              reindeer.copy(pos = reindeer.pos - reindeer.direction) -> 1, // backward step
              reindeer.copy(direction = reindeer.direction.left) -> 1000,
              reindeer.copy(direction = reindeer.direction.right) -> 1000,
            )
            if grid(oldReindeer.pos) != '#'
            oldDistance <- forwardResult.distances.get(oldReindeer)
            if oldDistance + step == distance // if step on shortest path
          } yield oldReindeer
        }
      }

      BFS.traverse(backwardTraversal).nodes.map(_.pos).size
    }
  }

  object AllPathsPart2Solution extends Part2Solution {
    override def bestPathTiles(grid: Grid[Char]): Int = {
      val forwardSearch = forwardGraphSearch(grid)
      val forwardResult = Dijkstra.searchAllPaths(forwardSearch)

      val backwardTraversal = new GraphTraversal[Reindeer] with UnitNeighbors[Reindeer] {
        override val startNode: Reindeer = forwardResult.target.get._1 // TODO: other orientations

        override def unitNeighbors(reindeer: Reindeer): IterableOnce[Reindeer] = forwardResult.allPrevNodes.getOrElse(reindeer, Set.empty)
      }

      BFS.traverse(backwardTraversal).nodes.map(_.pos).size
    }
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import AllPathsPart2Solution._
    println(lowestScore(parseGrid(input)))
    println(bestPathTiles(parseGrid(input)))
  }
}
