package eu.sim642.adventofcode2025

import eu.sim642.adventofcode2018.Day11.SumGrid
import eu.sim642.adventofcodelib.SeqImplicits.*
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.graph.{BFS, GraphTraversal, UnitNeighbors}
import eu.sim642.adventofcodelib.pos.Pos

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

  trait Part2Solution extends Part

  // Copied & modified from 2018 day 11
  class ArrayPartialSumGrid(f: Pos => Int, box: Box) extends SumGrid {
    val Box(min, max) = box
    val Pos(width, height) = max - min + Pos(1, 1)
    val partialSums: mutable.ArraySeq[mutable.ArraySeq[Int]] = mutable.ArraySeq.fill(height + 1, width + 1)(0) // larger by 1 to allow min-coordinate queries

    for (y <- 1 until height + 1) {
      for (x <- 1 until width + 1) {
        val pos = min + Pos(x - 1, y - 1)
        val sum = partialSums(y - 1)(x) + partialSums(y)(x - 1) - partialSums(y - 1)(x - 1) + f(pos)
        partialSums(y)(x) = sum
      }
    }

    override def sumBox(box: Box): Int = {
      //val Box(topLeft, bottomRight) = box
      val topLeft = box.min - min + Pos(1, 1)
      val bottomRight = box.max - min + Pos(1, 1)
      val bottomLeft1 = Pos(topLeft.x - 1, bottomRight.y)
      val topRight1 = Pos(bottomRight.x, topLeft.y - 1)
      val topLeft1 = Pos(topLeft.x - 1, topLeft.y - 1)
      partialSums(bottomRight.y)(bottomRight.x) - partialSums(bottomLeft1.y)(bottomLeft1.x) - partialSums(topRight1.y)(topRight1.x) + partialSums(topLeft1.y)(topLeft1.x)
    }
  }

  /**
   * Solution, which compresses the grid to 2-unit distances between occurring coordinates
   * and checks validity by counting outside cells (in the compressed grid) using a [[SumGrid]].
   *
   * Grid compression assumes no adjacent coordinates occur.
   */
  object CompressGridPart2Solution extends Part2Solution {
    def makeCompressPos(redTiles: Seq[Pos]): Pos => Pos = {
      val xs = redTiles.map(_.x).distinct.sorted
      val ys = redTiles.map(_.y).distinct.sorted

      p =>
        Pos(xs.indexOf(p.x) * 2 + 1, ys.indexOf(p.y) * 2 + 1) // leave edge around for fill
      // it's much faster to do the +1-s here than add Pos(1, 1) below
    }

    override def makeIsValid(redTiles: Seq[Pos]): Box => Boolean = {
      val compressPos = makeCompressPos(redTiles)
      def compressBox(box: Box): Box =
        Box(compressPos(box.min), compressPos(box.max))

      val originalBox = Box.bounding(redTiles)
      val compressedBox@Box(_, compressedMax) = compressBox(originalBox)

      val grid = mutable.ArraySeq.fill(compressedMax.y + 2, compressedMax.x + 2)(false) // leave edge around for fill
      // TODO: why need +2?
      for ((redTile1, redTile2) <- redTiles lazyZip redTiles.rotateLeft(1)) {
        for (pos <- compressBox(Box.bounding(Seq(redTile1, redTile2))).iterator)
          grid(pos.y)(pos.x) = true
      }
      //for (row <- grid) {
      //  for (cell <- row)
      //    print(cell)
      //  println()
      //}

      val graphTraversal = new GraphTraversal[Pos] with UnitNeighbors[Pos] {
        override val startNode: Pos = Pos.zero

        override def unitNeighbors(pos: Pos): IterableOnce[Pos] =
          Pos.axisOffsets.map(pos + _).filter(p => p.x >= 0 && p.y >= 0 && p.y < grid.size && p.x < grid(p.y).size && !grid(p.y)(p.x))
      }
      val outside = BFS.traverse(graphTraversal).nodes

      val outsideSumGrid = new ArrayPartialSumGrid(pos => if (outside(pos)) 1 else 0, compressedBox)
      def isValid(box: Box): Boolean = {
        val compressedBox = Box(compressPos(box.min), compressPos(box.max))
        //!compressedBox.iterator.exists(outside)
        outsideSumGrid.sumBox(compressedBox) == 0
      }

      isValid
    }
  }

  /**
   * Solution, which checks validity geometrically using axis-aligned line-box intersection.
   *
   * Assumes largest area won't be outside.
   * Assumes no adjacent coordinates occur.
   */
  object IntersectionPart2Solution extends Part2Solution {
    override def makeIsValid(redTiles: Seq[Pos]): Box => Boolean = {
      val lines =
        (redTiles lazyZip redTiles.rotateLeft(1))
          .toSeq
          .map((p1, p2) => Box.bounding(Seq(p1, p2))) // convert line to box of width/height 1

      def isValid(box: Box): Boolean = {
        // construct box interior, otherwise box intersects with its own edges
        val innerMin = box.min + Pos(1, 1)
        val innerMax = box.max - Pos(1, 1)
        //innerMin <= innerMax &&
        !(innerMin <= innerMax) || // box has no/empty interior // TODO: this is wrong for line-line intersections
          !lines.exists(_.intersect(Box(innerMin, innerMax)).nonEmpty) // interior doesn't intersect any line
      }

      isValid
    }
  }

  // TODO: line of sight in all directions solution (single iteration over corners, not all pairs!)
  // TODO: glguy's solution

  def parseRedTile(s: String): Pos = s match {
    case s"$x,$y" => Pos(x.toInt, y.toInt)
  }

  def parseRedTiles(input: String): Seq[Pos] = input.linesIterator.map(parseRedTile).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day9.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.largestArea(parseRedTiles(input)))
    println(CompressGridPart2Solution.largestArea(parseRedTiles(input)))
  }
}
