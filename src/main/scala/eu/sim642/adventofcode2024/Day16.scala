package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.graph.{BFS, Dijkstra, GraphSearch, GraphTraversal, TargetNode, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcode2018.Day13.DirectionPos

import scala.collection.mutable

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

  /*def bestPathTiles(grid: Grid[Char]): Int = {
    val targetScore = lowestScore(grid)

    val graphTraversal = new GraphTraversal[(List[Reindeer], Int)] {
      override val startNode: (List[Reindeer], Int) = (List(Reindeer(grid.posOf('S'))), 0)

      override def neighbors(reindeers: (List[Reindeer], Int)): IterableOnce[((List[Reindeer], Int), Int)] = {
        val reindeer = reindeers._1.head
        println(reindeers._2)
        Seq(
          reindeer.copy(pos = reindeer.pos + reindeer.direction) -> 1,
          reindeer.copy(direction = reindeer.direction.left) -> 1000,
          reindeer.copy(direction = reindeer.direction.right) -> 1000,
        )
          .filter(reindeer => grid(reindeer._1.pos) != '#')
          .filter(p => reindeers._2 + p._2 <= targetScore)
          .map(p => (p._1 :: reindeers._1, reindeers._2 + p._2) -> p._2)
      }

    }

    val targetPos: Pos = grid.posOf('E')

    Dijkstra.traverse(graphTraversal).nodes.filter(_._1.head.pos == targetPos).filter(_._2 == targetScore)
      .tapEach(println)
      .flatMap(_._1.map(_.pos)).toSet.size
  }*/

  /*def bestPathTiles(grid: Grid[Char]): Int = {
    val targetScore = lowestScore(grid)

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
    val targetPos: Pos = grid.posOf('E')

    val memo = mutable.Map.empty[(Reindeer, Set[Reindeer], Int), Set[Pos]]

    def helper(reindeer: Reindeer, visited: Set[Reindeer], distance: Int): Set[Pos] = {
      memo.getOrElseUpdate((reindeer, visited, distance), {
        if (distance == targetScore && reindeer.pos == targetPos)
          visited.map(_.pos)
        else if (distance >= targetScore)
          Set.empty
        else {
          assert(distance < targetScore)
          (for {
            (newReindeer, step) <- graphSearch.neighbors(reindeer)
            if !visited(newReindeer)
            newDistance = distance + step
            if newDistance <= targetScore
          } yield helper(newReindeer, visited + newReindeer, newDistance)).foldLeft(Set.empty)(_ ++ _)
        }
      })
    }

    val s = helper(graphSearch.startNode, Set.empty, 0)
    println(s)

    for ((row, y) <- grid.zipWithIndex) {
      for ((cell, x) <- row.zipWithIndex) {
        if (s(Pos(x, y)))
          print('O')
        else
          print(cell)
      }
      println()
    }

    s.size
  }*/

  def bestPathTiles(grid: Grid[Char]): Int = {
    val forwardSearch = forwardGraphSearch(grid)
    val forwardResult = Dijkstra.search(forwardSearch)

    val backwardTraversal = new GraphTraversal[Reindeer] with UnitNeighbors[Reindeer] {
      override val startNode: Reindeer = forwardResult.target.get._1 // TODO: other orientations

      override def unitNeighbors(reindeer: Reindeer): IterableOnce[Reindeer] = {
        val distance = forwardResult.distances(reindeer)
        for {
          (oldReindeer, step) <- Seq(
            reindeer.copy(pos = reindeer.pos - reindeer.direction) -> 1, // backward steo
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

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(lowestScore(parseGrid(input)))
    println(bestPathTiles(parseGrid(input)))
  }
}
