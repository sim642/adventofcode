package eu.sim642.adventofcode2018

import eu.sim642.adventofcode2017.Day3.Pos

object Day10 {

  case class Point(position: Pos, velocity: Pos) {
    def step: Point = Point(position + velocity, velocity)
  }

  private def boundingArea(positions: Seq[Pos]) = {
    val minX = positions.minBy(_.x).x
    val minY = positions.minBy(_.y).y
    val maxX = positions.maxBy(_.x).x
    val maxY = positions.maxBy(_.y).y
    val area = (maxX - minX).toLong * (maxY - minY).toLong
    area
  }

  def messageSecond(points: Seq[Point]): Int = {
    val steps = Stream.iterate(points)(_.map(_.step)).toIterator

    val (pts, sec) = steps.zipWithIndex.take(15000).minBy({ case (points, i) =>
      val positions = points.map(_.position)
      val area = boundingArea(positions)
      area
    })

    val (min, max) = Day6.boundingRect(pts.map(_.position))
    println(s"$min $max")
    println(boundingArea(pts.map(_.position)))
    val ptset = pts.map(_.position).toSet

    for (y <- min.y to max.y) {
      for (x <- min.x to max.x) {
        print(if (ptset.contains(Pos(x, y))) '#' else '.')
      }
      println()
    }

    sec
  }

  private val pointRegex = """position=<\s*(-?\d+),\s*(-?\d+)> velocity=<\s*(-?\d+),\s*(-?\d+)>""".r

  def parsePoint(s: String): Point = s match {
    case pointRegex(posX, posY, velX, velY) => Point(Pos(posX.toInt, posY.toInt), Pos(velX.toInt, velY.toInt))
  }

  def parsePoints(input: String): Seq[Point] = input.lines.map(parsePoint).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(messageSecond(parsePoints(input)))
  }
}
