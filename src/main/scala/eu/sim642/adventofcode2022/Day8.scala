package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day8 {

  trait Solution {
    def countVisibleTrees(grid: Grid[Int]): Int

    def maxScenicScore(grid: Grid[Int]): Int
  }

  extension (i: Int) {
    def orNotFound(x: => Int): Int = if (i < 0) x else i
  }

  object NaiveSolution extends Solution {

    // TODO: extract common
    def isVisible(grid: Grid[Int], gridTranspose: Grid[Int], pos: Pos): Boolean = {
      val row = grid(pos.y)
      val col = gridTranspose(pos.x)
      val cell = row(pos.x)
      val left = row.lastIndexWhere(_ >= cell, pos.x - 1)
      val right = row.indexWhere(_ >= cell, pos.x + 1)
      val top = col.lastIndexWhere(_ >= cell, pos.y - 1)
      val bottom = col.indexWhere(_ >= cell, pos.y + 1)
      left < 0 || right < 0 || top < 0 || bottom < 0
    }

    override def countVisibleTrees(grid: Grid[Int]): Int = {
      val gridTranspose = grid.transpose
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if isVisible(grid, gridTranspose, Pos(x, y))
      } yield ()).size
    }

    def scenicScore(grid: Grid[Int], gridTranspose: Grid[Int], pos: Pos): Int = {
      val row = grid(pos.y)
      val col = gridTranspose(pos.x)
      val cell = row(pos.x)
      val left = pos.x - row.lastIndexWhere(_ >= cell, pos.x - 1).orNotFound(0)
      val right = row.indexWhere(_ >= cell, pos.x + 1).orNotFound(row.size - 1) - pos.x
      val top = pos.y - col.lastIndexWhere(_ >= cell, pos.y - 1).orNotFound(0)
      val bottom = col.indexWhere(_ >= cell, pos.y + 1).orNotFound(grid.size - 1) - pos.y
      left * right * top * bottom
    }

    override def maxScenicScore(grid: Grid[Int]): Int = {
      val gridTranspose = grid.transpose
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
      } yield scenicScore(grid, gridTranspose, Pos(x, y))).max
    }
  }

  object PrefixSolution extends Solution {

    // TODO: extract common
    override def countVisibleTrees(grid: Grid[Int]): Int = {
      val gridTranspose = grid.transpose
      // prefix maximums from edges
      val left = grid.map(_.scanLeft(-1)(_ max _))
      val right = grid.map(_.scanRight(-1)(_ max _))
      val top = gridTranspose.map(_.scanLeft(-1)(_ max _))
      val bottom = gridTranspose.map(_.scanRight(-1)(_ max _))
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        if cell > left(y)(x) || cell > right(y)(x + 1) || cell > top(x)(y) || cell > bottom(x)(y + 1)
      } yield ()).size
    }

    override def maxScenicScore(grid: Grid[Int]): Int = {
      val gridTranspose = grid.transpose
      // prefix height -> index maps from edges
      val init = Map.empty[Int, Int]

      def op(acc: Map[Int, Int], p: (Int, Int)): Map[Int, Int] = {
        val (cell, i) = p
        acc ++ (0 to cell).map(_ -> i)
      }

      val left = grid.map(_.zipWithIndex.scanLeft(init)(op))
      val right = grid.map(_.reverse.zipWithIndex.scanLeft(init)(op).reverse)
      val top = gridTranspose.map(_.zipWithIndex.scanLeft(init)(op))
      val bottom = gridTranspose.map(_.reverse.zipWithIndex.scanLeft(init)(op).reverse)
      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        l = x - left(y)(x).getOrElse(cell, 0)
        r = row.size - 1 - right(y)(x + 1).getOrElse(cell, 0) - x
        t = y - top(x)(y).getOrElse(cell, 0)
        b = grid.size - 1 - bottom(x)(y + 1).getOrElse(cell, 0) - y
      } yield l * r * t * b).max
    }
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(PrefixSolution.countVisibleTrees(parseGrid(input)))
    println(NaiveSolution.maxScenicScore(parseGrid(input)))
  }
}
