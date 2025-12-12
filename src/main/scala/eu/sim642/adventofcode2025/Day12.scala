package eu.sim642.adventofcode2025

import eu.sim642.adventofcodelib.Grid
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits.*
import eu.sim642.adventofcode2020.Day20.OrientationGridOps
import eu.sim642.adventofcodelib.GridImplicits._
import eu.sim642.adventofcodelib.box.Box

object Day12 {

  type Shape = Grid[Boolean]

  case class Region(size: Pos, shapeCounts: Seq[Int])

  case class Input(shapes: Seq[Shape], regions: Seq[Region])

  trait Solution {
    def fits(shapes: Seq[Shape])(region: Region): Boolean

    def countFits(input: Input): Int =
      input.regions.count(fits(input.shapes))
  }

  object NaiveSolution extends Solution {
    /*def fits(shapes: Seq[Shape])(region: Region): Boolean = {

    def helper(grid: Grid[Boolean], shapeCounts: Seq[Int]): Boolean = {
      val shapeI = shapeCounts.indexWhere(_ > 0)
      if (shapeI < 0)
        true // nothing more to fit
      else {
        val newShapeCounts = shapeCounts.updated(shapeI, shapeCounts(shapeI) - 1)
        val shape = shapes(shapeI)
        (for {
          shapeOrientation <- shape.orientations.iterator
          shapeOrientationSize = Pos(shapeOrientation(0).size, shapeOrientation.size)
          shapeOrientationBox = Box(Pos.zero, region.size - shapeOrientationSize)
          shapeOrientationBox2 = Box(Pos.zero, shapeOrientationSize - Pos(1, 1))
          pos <- shapeOrientationBox.iterator
          if shapeOrientationBox2.iterator.forall(p => !(shapeOrientation(p) && grid(pos + p)))
          newGrid = shapeOrientationBox2.iterator.foldLeft(grid)((newGrid, p) => newGrid.updatedGrid(pos + p, true))
        } yield helper(newGrid, newShapeCounts)).exists(identity)
      }
    }

    val initialGrid = Vector.fill(region.size.y, region.size.x)(false)
    helper(initialGrid, region.shapeCounts)
  }*/

    // probably broken: skips over poss when finding place for a shape
    override def fits(shapes: Seq[Shape])(region: Region): Boolean = {
      val shapeOrientationBox = Box(Pos.zero, region.size - Pos(3, 3)) // assuming all shappes of same size

      def helper(grid: Grid[Boolean], shapeCounts: Seq[Int], poss: List[Pos]): Boolean = {
        val shapeI = shapeCounts.indexWhere(_ > 0)
        if (shapeI < 0)
          true // nothing more to fit
        else {
          val newShapeCounts = shapeCounts.updated(shapeI, shapeCounts(shapeI) - 1)
          val shape = shapes(shapeI)
          (for {
            shapeOrientation <- shape.orientations.iterator
            shapeOrientationSize = Pos(shapeOrientation(0).size, shapeOrientation.size)
            shapeOrientationBox2 = Box(Pos.zero, shapeOrientationSize - Pos(1, 1))
            case pos :: newPoss <- poss.tails
            if shapeOrientationBox2.iterator.forall(p => !(shapeOrientation(p) && grid(pos + p)))
            newGrid = shapeOrientationBox2.iterator.foldLeft(grid)((newGrid, p) => newGrid.updatedGrid(pos + p, shapeOrientation(p)))
          } yield helper(newGrid, newShapeCounts, newPoss)).exists(identity)
        }
      }

      val initialGrid = Vector.fill(region.size.y, region.size.x)(false)
      helper(initialGrid, region.shapeCounts, shapeOrientationBox.iterator.toList)
    }

    /*def fits(shapes: Seq[Shape])(region: Region): Boolean = {
    val shapeOrientationBox = Box(Pos.zero, region.size - Pos(3, 3)) // assuming all shappes of same size

    def helper(grid: Grid[Boolean], shapeCounts: Seq[Int], poss: List[Pos]): Boolean = {
      val shapeI = shapeCounts.indexWhere(_ > 0)
      if (shapeI < 0)
        true // nothing more to fit
      else {
        //val newShapeCounts = shapeCounts.updated(shapeI, shapeCounts(shapeI) - 1)
        //val shape = shapes(shapeI)
        if (poss.isEmpty)
          false
        else {
          val pos = poss.head
          val newPoss = poss.tail
          (for {
            ((shape, count), shapeI) <- shapes.lazyZip(shapeCounts).zipWithIndex
            if count > 0
            newShapeCounts = shapeCounts.updated(shapeI, count - 1)
            shapeOrientation <- shape.orientations.iterator
            shapeOrientationSize = Pos(shapeOrientation(0).size, shapeOrientation.size)
            shapeOrientationBox2 = Box(Pos.zero, shapeOrientationSize - Pos(1, 1))
            if shapeOrientationBox2.iterator.forall(p => !(shapeOrientation(p) && grid(pos + p)))
            newGrid = shapeOrientationBox2.iterator.foldLeft(grid)((newGrid, p) => newGrid.updatedGrid(pos + p, shapeOrientation(p)))
          } yield helper(newGrid, newShapeCounts, newPoss)).exists(identity) || helper(grid, shapeCounts, newPoss) // or don't place anything here
        }
      }
    }

    val initialGrid = Vector.fill(region.size.y, region.size.x)(false)
    helper(initialGrid, region.shapeCounts, shapeOrientationBox.iterator.toList)
  }*/
  }

  object SanitySolution extends Solution {
    override def fits(shapes: Seq[Shape])(region: Region): Boolean = {
      val area = Box(Pos(1, 1), region.size).size[Int]
      val areaLowerBound = // only counting the #-s in shapes
        shapes
          .map(_.countGrid(identity))
          .lazyZip(region.shapeCounts)
          .map(_ * _)
          .sum
      val areaUpperBound = // counting area needed if no "overlaps" are possible
        shapes
          .map(_.sizeGrid)
          .lazyZip(region.shapeCounts)
          .map(_ * _)
          .sum
      if (areaUpperBound <= area)
        true
      else if (areaLowerBound > area)
        false
      else
        throw IllegalArgumentException("undecidable by sanity check")
    }
  }

  def parseShape(s: String): Shape = s.linesIterator.tail.map(_.map(_ == '#').toVector).toVector

  def parseRegion(s: String): Region = s match {
    case s"${width}x$height: $shapeCounts" =>
      Region(Pos(width.toInt, height.toInt), shapeCounts.split(" ").map(_.toInt).toSeq)
  }

  def parseInput(input: String): Input = {
    val lineGroups = input.split("\n\n").toSeq
    Input(lineGroups.init.map(parseShape), lineGroups.last.linesIterator.map(parseRegion).toSeq)
  }

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(SanitySolution.countFits(parseInput(input)))
  }
}
