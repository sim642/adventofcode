package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos


object Day10 {

  def countTrails(grid: Grid[Char], startPos: Pos): Int = {
    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
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

    BFS.traverse(graphTraversal).nodes.count(grid(_) == '9')
  }

  def countTrailPaths(grid: Grid[Char], startPos: Pos): Int = {
    // TODO: better way of counting all paths than just BFS on path nodes?
    val graphTraversal = new GraphTraversal[List[Pos]] with UnitNeighbors[List[Pos]] {
      override val startNode: List[Pos] = List(startPos)

      // TODO: deduplicate
      override def unitNeighbors(node: List[Pos]): IterableOnce[List[Pos]] = {
        val pos = node.head
        val height = grid(pos)
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) == height + 1
        } yield newPos :: node
      }
    }

    BFS.traverse(graphTraversal).nodes.count(node => grid(node.head) == '9')
  }

  def sumTrailheadScores(grid: Grid[Char]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '0'
    } yield countTrails(grid, Pos(x, y))).sum
  }

  // TODO: deduplicate
  def sumTrailheadRatings(grid: Grid[Char]): Int = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '0'
    } yield countTrailPaths(grid, Pos(x, y))).sum
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumTrailheadScores(parseGrid(input)))
    println(sumTrailheadRatings(parseGrid(input)))
  }
}
