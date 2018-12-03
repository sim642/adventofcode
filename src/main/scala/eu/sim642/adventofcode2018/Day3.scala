package eu.sim642.adventofcode2018

import scala.collection.mutable
import scala.language.implicitConversions

object Day3 {

  case class Rectangle(id: Int, left: Int, top: Int, width: Int, height: Int)

  private val rectangleRegex = """#(\d+) @ (\d+),(\d+): (\d+)x(\d+)""".r

  def parseRectangle(s: String): Rectangle = s match {
    case rectangleRegex(id, left, top, width, height) => Rectangle(id.toInt, left.toInt, top.toInt, width.toInt, height.toInt)
  }

  def parseRectangles(input: String): Seq[Rectangle] = input.lines.map(parseRectangle).toSeq

  def overlappingFabric(rects: Seq[Rectangle]): Int = {
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


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day3.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(overlappingFabric(parseRectangles(input)))
  }
}
