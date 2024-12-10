package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos


object Day10 {

  def trailheads(grid: Grid[Char]): Iterable[Pos] = {
    for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '0'
    } yield Pos(x, y)
  }

  def trailTraversal(grid: Grid[Char], startPos: Pos): GraphTraversal[Pos] & UnitNeighbors[Pos] = {
    new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = startPos

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        val height = grid(pos)
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) == height + 1
        } yield newPos
      }
    }
  }

  def trailheadScore(grid: Grid[Char])(startPos: Pos): Int = { // aka countTrails
    val graphTraversal = trailTraversal(grid, startPos)
    BFS.traverse(graphTraversal).nodes.count(grid(_) == '9')
  }

  def sumTrailheadScores(grid: Grid[Char]): Int =
    trailheads(grid).map(trailheadScore(grid)).sum

  // TODO: extract to library?
  def pathTraversal[A](graphTraversal: GraphTraversal[A] & UnitNeighbors[A]): GraphTraversal[List[A]] & UnitNeighbors[List[A]] = {
    new GraphTraversal[List[A]] with UnitNeighbors[List[A]] {
      override val startNode: List[A] = List(graphTraversal.startNode)

      override def unitNeighbors(node: List[A]): IterableOnce[List[A]] =
        graphTraversal.unitNeighbors(node.head).iterator.map(_ :: node)
    }
  }

  def trailheadRating(grid: Grid[Char])(startPos: Pos): Int = { // aka countTrailPaths
    // TODO: better way of counting all paths than just BFS on path nodes?
    val graphTraversal = pathTraversal(trailTraversal(grid, startPos))
    BFS.traverse(graphTraversal).nodes.count(node => grid(node.head) == '9')
  }

  def sumTrailheadRatings(grid: Grid[Char]): Int =
    trailheads(grid).map(trailheadRating(grid)).sum

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumTrailheadScores(parseGrid(input)))
    println(sumTrailheadRatings(parseGrid(input)))
  }
}
