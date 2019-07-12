package eu.sim642.adventofcode2018

import scala.collection.mutable

object Day3 {

  case class Rectangle(id: Int, left: Int, top: Int, width: Int, height: Int)

  private val rectangleRegex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  def parseRectangle(s: String): Rectangle = s match {
    case rectangleRegex(id, left, top, width, height) => Rectangle(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
  }

  def parseRectangles(input: String): Seq[Rectangle] = input.lines.map(parseRectangle).toSeq

  def overlappingFabric(rects: Seq[Rectangle]): Int = {
    // TODO: nasty mutable
    val overlaps: mutable.Map[(Int, Int), Int] = mutable.Map.empty.withDefaultValue(0)

    for {
      Rectangle(_, left, top, width, height) <- rects.toSet
      x <- left until (left + width)
      y <- top until (top + height)
    } {
      val pos = (x, y)
      overlaps(pos) += 1
    }

    overlaps.values.count(_ >= 2)
  }

  def nonOverlappingRectangle(rects: Seq[Rectangle]): Int = {
    // TODO: duplication
    val overlaps: mutable.Map[(Int, Int), Rectangle] = mutable.Map.empty
    val nonOverlaps: mutable.Set[Rectangle] = rects.to(mutable.Set)

    for {
      rect@Rectangle(_, left, top, width, height) <- rects.toSet
      x <- left until (left + width)
      y <- top until (top + height)
    } {
      val pos = (x, y)
      if (overlaps.contains(pos)) {
        nonOverlaps -= rect
        nonOverlaps -= overlaps(pos)
      }
      else
        overlaps(pos) = rect
    }

    nonOverlaps.head.id
  }


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(overlappingFabric(parseRectangles(input)))
    println(nonOverlappingRectangle(parseRectangles(input)))
  }
}
