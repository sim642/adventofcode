package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

object Day9 {

  def lowPoints(grid: Grid[Int]): Seq[Pos] = {
    (for {
      (row, y) <- grid.view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      pos = Pos(x, y)
      neighbors = Pos.axisOffsets.map(pos + _).filter(grid.containsPos)
      if neighbors.forall(grid(_) > cell)
    } yield pos).toSeq
  }

  def sumLowPointRiskLevels(grid: Grid[Int]): Int = {
    val lowPointHeights = lowPoints(grid).map(grid(_))
    lowPointHeights.map(_ + 1).sum
  }


  def basinSize(grid: Grid[Int])(lowPoint: Pos): Int = {
    val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
      override val startNode: Pos = lowPoint

      override def unitNeighbors(pos: Pos): IterableOnce[Pos] = {
        for {
          offset <- Pos.axisOffsets
          newPos = pos + offset
          if grid.containsPos(newPos)
          if grid(newPos) < 9
          // 9s are not in any basin and others are in exactly one basin.
          // This means basins are always delimited by 9s, nothing else,
          // because lower ridges would be part of multiple basins.
        } yield newPos
      }
    }

    BFS.traverse(graphTraversal).nodes.size
  }

  def multiplyTopBasins(grid: Grid[Int]): Int = {
    lowPoints(grid)
      .map(basinSize(grid))
      .sorted(using Ordering.Int.reverse)
      .take(3)
      .product
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector.map(_.asDigit)).toVector

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(sumLowPointRiskLevels(parseGrid(input)))
    println(multiplyTopBasins(parseGrid(input)))
  }
}
