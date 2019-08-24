package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.TraversableOnceImplicits._

object Day6 {

  def largestFiniteArea(coords: Seq[Pos]): Int = {
    def closestCoord(pos: Pos): Option[Pos] = {
      coords.minStrict(Ordering.by(_ manhattanDistance pos))
    }

    val box@Box(min, max) = Box.bounding(coords)

    // TODO: optimize with BFS
    val grid = (for {
      pos <- box.iterator
      coord <- closestCoord(pos)
    } yield pos -> coord).toMap

    val finiteGrid = grid.filterKeys(pos => pos.x != min.x && pos.x != max.x && pos.y != min.y && pos.y != max.y)
    val finiteCoordSizes = finiteGrid.groupBy(_._2).mapValues(_.size)
    finiteCoordSizes.values.max
  }

  def safeArea(coords: Seq[Pos], safeDistance: Int = 10000): Int = {
    def totalDistance(pos: Pos): Int = coords.map(_ manhattanDistance pos).sum

    val box = Box.bounding(coords)
    box.iterator.count(totalDistance(_) < safeDistance)
  }

  private val coordRegex = """(\d+), (\d+)""".r

  def parseCoord(s: String): Pos = s match {
    case coordRegex(x, y) => Pos(x.toInt, y.toInt)
  }

  def parseCoords(input: String): Seq[Pos] = input.lines.map(parseCoord).toSeq


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day6.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(largestFiniteArea(parseCoords(input)))
    println(safeArea(parseCoords(input)))
  }
}
