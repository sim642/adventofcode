package eu.sim642.adventofcode2025

import eu.sim642.adventofcode2018.Day11.SumGrid
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.SeqImplicits.*
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}

import scala.collection.immutable.SortedSet
import scala.collection.mutable

object Day9 {

  trait Part {
    def makeIsValid(redTiles: Seq[Pos]): Box => Boolean

    def largestArea(redTiles: Seq[Pos]): Long = {
      val isValid = makeIsValid(redTiles)

      (for {
        // faster than combinations(2)
        (p1, i) <- redTiles.iterator.zipWithIndex
        p2 <- redTiles.view.slice(i + 1, redTiles.size).iterator
        box = Box.bounding(Seq(p1, p2))
        if isValid(box)
      } yield box.size[Long]).max
    }
  }

  object Part1 extends Part {
    override def makeIsValid(redTiles: Seq[Pos]): Box => Boolean =
      _ => true
  }

  // Copied & modified from 2018 day 11
  class ArrayPartialSumGrid(f: Pos => Int, box: Box) extends SumGrid {
    val Box(min, max) = box
    val Pos(width, height) = max - min + Pos(1, 1)
    val partialSums: mutable.ArraySeq[mutable.ArraySeq[Int]] = mutable.ArraySeq.fill(height, width)(0)
    // TODO: should be larger by 1 to allow min-coordinate queries?

    for (y <- 0 until height) {
      for (x <- 0 until width) {
        val pos = min + Pos(x, y)
        val sum =
          (if (y >= 1) partialSums(y - 1)(x) else 0) +
            (if (x >= 1) partialSums(y)(x - 1) else 0) -
            (if (x >= 1 && y >= 1) partialSums(y - 1)(x - 1) else 0) +
            f(pos)
        partialSums(y)(x) = sum
      }
    }

    override def sumBox(box: Box): Int = {
      //val Box(topLeft, bottomRight) = box
      val topLeft = box.min - min
      val bottomRight = box.max - min
      val bottomLeft1 = Pos(topLeft.x - 1, bottomRight.y)
      val topRight1 = Pos(bottomRight.x, topLeft.y - 1)
      val topLeft1 = Pos(topLeft.x - 1, topLeft.y - 1)
      partialSums(bottomRight.y)(bottomRight.x) - partialSums(bottomLeft1.y)(bottomLeft1.x) - partialSums(topRight1.y)(topRight1.x) + partialSums(topLeft1.y)(topLeft1.x)
    }
  }

  object Part2 extends Part {
    override def makeIsValid(redTiles: Seq[Pos]): Box => Boolean = {
      // TODO: clean up
      // TODO: optimize (with polygon checks?)
      val xs = redTiles.map(_.x).distinct.sorted
      val ys = redTiles.map(_.y).distinct.sorted
      //println(xs)
      //println(ys)

      def mapPos(p: Pos): Pos =
        Pos(xs.indexOf(p.x) * 2 + 1, ys.indexOf(p.y) * 2 + 1) // leave edge around for fill

      val grid = mutable.ArraySeq.fill(ys.size * 2 - 1 + 2, xs.size * 2 - 1 + 2)('.')

      for ((redTile1, redTile2) <- redTiles lazyZip redTiles.rotateLeft(1)) {
        val gridPos1 = mapPos(redTile1)
        val gridPos2 = mapPos(redTile2)
        for (gridPos <- Box.bounding(Seq(gridPos1, gridPos2)).iterator)
          grid(gridPos.y)(gridPos.x) = 'X'
      }

      for (redTile <- redTiles) {
        val gridPos = mapPos(redTile)
        //println((redTile, gridPos))
        grid(gridPos.y)(gridPos.x) = '#'
      }

      //for (row <- grid) {
      //  for (cell <- row)
      //    print(cell)
      //  println()
      //}

      val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
        override val startNode: Pos = Pos.zero

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] =
          Pos.axisOffsets.map(pos + _).filter(p => p.x >= 0 && p.y >= 0 && p.y < grid.size && p.x < grid(p.y).size && grid(p.y)(p.x) == '.')
      }

      val outside = BFS.traverse(graphTraversal).nodes

      val outsideSumGrid = new ArrayPartialSumGrid(pos => if (outside(pos)) 1 else 0, Box(Pos.zero, Pos(xs.size * 2, ys.size * 2)))

      def isValid(box: Box): Boolean = {
        val gridBox = Box(mapPos(box.min), mapPos(box.max))
        //!gridBox.iterator.exists(outside)
        outsideSumGrid.sumBox(gridBox) == 0
      }

      isValid
    }
  }

  /*object Part2 extends Part {
    override def largestArea(redTiles: Seq[Pos]): Long = {

      def isValid(box: Box): Boolean = {
        val boxCorners = Seq(box.min, Pos(box.max.x, box.min.y), box.max, Pos(box.min.x, box.max.y))
        ???
      }

      (for {
        // faster than combinations(2)
        (p1, i) <- redTiles.iterator.zipWithIndex
        p2 <- redTiles.view.slice(i + 1, redTiles.size).iterator
        box = Box.bounding(Seq(p1, p2))
        if isValid(box)
      } yield box.size[Long]).max
    }
  }*/

  def parseRedTile(s: String): Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }

  def parseRedTiles(input: String): Seq[Pos] = input.linesIterator.map(parseRedTile).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.largestArea(parseRedTiles(input)))
    println(Part2.largestArea(parseRedTiles(input)))
  }
}
