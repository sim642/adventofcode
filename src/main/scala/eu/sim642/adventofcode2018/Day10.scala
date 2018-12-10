package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos
import eu.sim642.adventofcode2017.Day19.Pos2

object Day10 {

  case class Point(position: Pos, velocity: Pos) {
    def step: Point = Point(position + velocity, velocity)
  }

  def boundingArea(positions: Seq[Pos]): Long = {
    val (min, max) = Day6.boundingRect(positions)
    val delta = max + (-min)
    val area = delta.x.toLong * delta.y.toLong
    area
  }

  def minimizePointsArea(points: Seq[Point]): (Seq[Point], Int) = {
    val steps = Iterator.iterate(points)(_.map(_.step))

    val (minPoints, minSecond) = steps.zipWithIndex.take(15000).minBy({ case (points, second) =>
      val positions = points.map(_.position)
      boundingArea(positions)
    })

    (minPoints, minSecond)
  }

  def printPoints(points: Seq[Point]): Unit = {
    val positions = points.map(_.position)
    val (min, max) = Day6.boundingRect(positions)
    val positionsSet = positions.toSet

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        val pos = Pos(x, y)
        print(if (positionsSet.contains(pos)) '#' else '.')
      }
      println()
    }
  }

  private val pointRegex = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

  def parsePoint(s: String): Point = s match {
    case pointRegex(posX, posY, velX, velY) => Point(Pos(posX.toInt, posY.toInt), Pos(velX.toInt, velY.toInt))
  }

  def parsePoints(input: String): Seq[Point] = input.lines.map(parsePoint).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    val (minPoints, minSecond) = minimizePointsArea(parsePoints(input))
    printPoints(minPoints)
    println(minSecond)
  }
}
