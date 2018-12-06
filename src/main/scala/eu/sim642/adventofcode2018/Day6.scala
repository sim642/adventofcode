package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos

object Day6 {

  def boundingRect(coords: Seq[Pos]): (Pos, Pos) = {
    val minX = coords.minBy(_.x).x
    val minY = coords.minBy(_.y).y
    val maxX = coords.maxBy(_.x).x
    val maxY = coords.maxBy(_.y).y
    (Pos(minX, minY), Pos(maxX, maxY))
  }

  def iterateRect(min: Pos, max: Pos): Iterator[Pos] = {
    for {
      x <- (min.x to max.x).toIterator
      y <- (min.y to max.y).toIterator
    } yield Pos(x, y)
  }

  def largestFiniteArea(coords: Seq[Pos]): Int = {
    def closestCoord(pos: Pos): Option[Pos] = {
      val Seq((coord1, d1), (coord2, d2)) = coords.toSeq.map(coord => coord -> (coord manhattanDistance pos)).sortBy(_._2).take(2) // TODO: optimize taking only smallest 2
      if (d1 < d2)
        Some(coord1)
      else
        None
    }

    val (min, max) = boundingRect(coords)

    // TODO: optimize with BFS
    val grid = (for {
      pos <- iterateRect(min, max)
      coord <- closestCoord(pos)
    } yield pos -> coord).toMap

    val finiteGrid = grid.filterKeys(pos => pos.x != min.x && pos.x != max.x && pos.y != min.y && pos.y != max.y)
    val finiteCoordSizes = finiteGrid.groupBy(_._2).mapValues(_.size)
    finiteCoordSizes.values.max
  }

  def safeArea(coords: Seq[Pos], safeDistance: Int = 10000): Int = {
    def totalDistance(pos: Pos): Int = coords.map(_ manhattanDistance pos).sum

    val (min, max) = boundingRect(coords)
    iterateRect(min, max).count(totalDistance(_) < safeDistance)
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
