package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos

object Day6 {

  def largestFiniteArea(coords: Seq[Pos]): Int = {
    def closestCoord(pos: Pos): Option[Pos] = {
      val Seq((coord1, d1), (coord2, d2)) = coords.toSeq.map(coord => coord -> (coord manhattanDistance pos)).sortBy(_._2).take(2) // TODO: optimize taking only smallest 2
      if (d1 < d2)
        Some(coord1)
      else
        None
    }

    val minX = coords.minBy(_.x).x
    val minY = coords.minBy(_.y).y
    val maxX = coords.maxBy(_.x).x
    val maxY = coords.maxBy(_.y).y

    // TODO: optimize with BFS
    val grid = (for {
      x <- minX to maxX
      y <- minY to maxY
      pos = Pos(x, y)
      coord <- closestCoord(pos)
    } yield pos -> coord).toMap

    val finiteGrid = grid.filterKeys(pos => pos.x != minX && pos.x != maxX && pos.y != minY && pos.y != maxY)
    val finiteSizes = finiteGrid.groupBy(_._2).mapValues(_.size)
    finiteSizes.values.max
  }

  def safeArea(coords: Seq[Pos], safeDistance: Int = 10000): Int = {
    def totalDistance(pos: Pos): Int = {
      coords.map(coord => coord manhattanDistance pos).sum
    }

    val minX = coords.minBy(_.x).x
    val minY = coords.minBy(_.y).y
    val maxX = coords.maxBy(_.x).x
    val maxY = coords.maxBy(_.y).y

    (for {
      x <- minX to maxX
      y <- minY to maxY
      pos = Pos(x, y)
      total = totalDistance(pos)
      if total < safeDistance
    } yield pos).size
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
