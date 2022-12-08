package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.{FenwickTree, Grid}
import eu.sim642.adventofcodelib.GridImplicits.*
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec
import scala.collection.SeqView
import scala.collection.immutable.ArraySeq

object Day8 {

  trait Solution {
    def countVisibleTrees(grid: Grid[Int]): Int

    def maxScenicScore(grid: Grid[Int]): Int
  }

  /**
   * Solution, which is based on furthest visible indices for a position.
   */
  trait VisibleIndicesSolution extends Solution {

    /**
     * Furthest visible indices.
     * @param left x coordinate, <0 if non-existent
     * @param right x coordinate, >=width if non-existent
     * @param top y coordinate, <0 if non-existent
     * @param bottom y coordinate, >=height if non-existent
     */
    case class VisibleIndices(left: Int, right: Int, top: Int, bottom: Int) {
      def clamp(width: Int, height: Int): VisibleIndices = {
        VisibleIndices(
          left = left max 0,
          right = right min (width - 1),
          top = top max 0,
          bottom = bottom min (height - 1)
        )
      }
    }

    /**
     * Makes a visible indices function for a grid.
     */
    def makeVisibleIndices(grid: Grid[Int]): Pos => VisibleIndices

    override def countVisibleTrees(grid: Grid[Int]): Int = {
      val width = grid(0).size
      val height = grid.size
      val visibleIndices = makeVisibleIndices(grid)

      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
        VisibleIndices(left, right, top, bottom) = visibleIndices(Pos(x, y))
        if left < 0 || right >= width || top < 0 || bottom >= height
      } yield ()).size // TODO: countGridWithPos
    }

    override def maxScenicScore(grid: Grid[Int]): Int = {
      val width = grid(0).size
      val height = grid.size
      val visibleIndices = makeVisibleIndices(grid)

      (for {
        (row, y) <- grid.view.zipWithIndex
        (cell, x) <- row.view.zipWithIndex
      } yield {
        val VisibleIndices(left, right, top, bottom) = visibleIndices(Pos(x, y)).clamp(width, height)
        (x - left) * (right - x) * (y - top) * (bottom - y)
      }).max // TODO: mapGridWithPos, maxGrid
    }
  }

  extension (i: Int) {
    def orNotFound(x: => Int): Int = if (i < 0) x else i
  }

  /**
   * Solution, which finds furthest visible indices naively each time.
   */
  object NaiveSolution extends Solution with VisibleIndicesSolution {

    override def makeVisibleIndices(grid: Grid[Int]): Pos => VisibleIndices = {
      val gridTranspose = grid.transpose
      val width = gridTranspose.size
      val height = grid.size

      def visibleIndices(pos: Pos): VisibleIndices = {
        val Pos(x, y) = pos
        val row = grid(y)
        val col = gridTranspose(x)
        val cell = row(x)
        val left = row.lastIndexWhere(_ >= cell, x - 1)
        val right = row.indexWhere(_ >= cell, x + 1).orNotFound(width)
        val top = col.lastIndexWhere(_ >= cell, y - 1)
        val bottom = col.indexWhere(_ >= cell, y + 1).orNotFound(height)
        VisibleIndices(left, right, top, bottom)
      }

      visibleIndices
    }
  }

  /**
   * Solution, which precomputes furthest visible indices for prefixes in each direction.
   * Prefix direction elements are mappings from tree height, also prefix-expanded for >=cell lookup.
   */
  trait PrefixSolution extends Solution with VisibleIndicesSolution {

    override def makeVisibleIndices(grid: Grid[Int]): Pos => VisibleIndices = {
      val gridTranspose = grid.transpose
      val width = gridTranspose.size
      val height = grid.size

      def opLeft(acc: Seq[Int], cellIndex: (Int, Int)): Seq[Int] = {
        val (cell, i) = cellIndex
        //ArraySeq.fill(cell + 1)(i) ++ acc.drop(cell + 1)
        //(Iterator.fill(cell + 1)(i) ++ acc.view.drop(cell + 1)).to(acc.iterableFactory)
        acc.iterableFactory.fill(cell + 1)(i) ++ acc.drop(cell + 1)
      }

      def opRight(cellIndex: (Int, Int), acc: Seq[Int]): Seq[Int] = opLeft(acc, cellIndex)

      val lefts = grid.map(_.view.zipWithIndex.scanLeft(Vector.fill(10)(-1))(opLeft).toVector)
      val rights = grid.map(_.view.zipWithIndex.scanRight(Vector.fill(10)(width))(opRight).toVector)
      val tops = gridTranspose.map(_.view.zipWithIndex.scanLeft(Vector.fill(10)(-1))(opLeft).toVector)
      val bottoms = gridTranspose.map(_.view.zipWithIndex.scanRight(Vector.fill(10)(height))(opRight).toVector)

      def visibleIndices(pos: Pos): VisibleIndices = {
        val Pos(x, y) = pos
        val cell = grid(pos)
        val left = lefts(y)(x)(cell)
        val right = rights(y)(x + 1)(cell)
        val top = tops(x)(y)(cell)
        val bottom = bottoms(x)(y + 1)(cell)
        VisibleIndices(left, right, top, bottom)
      }

      visibleIndices
    }
  }

  object PrefixSolution extends PrefixSolution

  /**
   * Solution, which precomputes furthest visible indices for prefixes in each direction.
   * Prefix direction elements are mappings from tree height, using Fenwick trees.
   */
  object FenwickPrefixSolution extends Solution with VisibleIndicesSolution {

    override def makeVisibleIndices(grid: Grid[Int]): Pos => VisibleIndices = {
      val gridTranspose = grid.transpose
      val width = gridTranspose.size
      val height = grid.size

      def opLeft(acc: FenwickTree[Int], cellIndex: (Int, Int)): FenwickTree[Int] = {
        val (cell, i) = cellIndex
        acc.updated(9 - cell, i)
      }

      def opRight(cellIndex: (Int, Int), acc: FenwickTree[Int]): FenwickTree[Int] = opLeft(acc, cellIndex)

      val lefts = grid.map(_.view.zipWithIndex.scanLeft(FenwickTree.fill(10)(-1)(using Integer.max))(opLeft).toVector)
      val rights = grid.map(_.view.zipWithIndex.scanRight(FenwickTree.fill(10)(width)(using Integer.min))(opRight).toVector)
      val tops = gridTranspose.map(_.view.zipWithIndex.scanLeft(FenwickTree.fill(10)(-1)(using Integer.max))(opLeft).toVector)
      val bottoms = gridTranspose.map(_.view.zipWithIndex.scanRight(FenwickTree.fill(10)(height)(using Integer.min))(opRight).toVector)

      def visibleIndices(pos: Pos): VisibleIndices = {
        val Pos(x, y) = pos
        val cell = grid(pos)
        val left = lefts(y)(x)(9 - cell)
        val right = rights(y)(x + 1)(9 - cell)
        val top = tops(x)(y)(9 - cell)
        val bottom = bottoms(x)(y + 1)(9 - cell)
        VisibleIndices(left, right, top, bottom)
      }

      visibleIndices
    }
  }

  /**
   * Solution, which optimizes part 1 by only computing maximum heights for direction prefixes.
   */
  object OptimizedPrefixSolution extends PrefixSolution {

    override def countVisibleTrees(grid: Grid[Int]): Int = {
      val gridTranspose = grid.transpose
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
  }


  def parseGrid(input: String): Grid[Int] = input.linesIterator.map(_.toVector).toVector.mapGrid(_.asDigit)

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day8.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import OptimizedPrefixSolution._

    println(countVisibleTrees(parseGrid(input)))
    println(maxScenicScore(parseGrid(input)))
  }
}
