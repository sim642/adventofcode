package eu.sim642.adventofcode2018

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day10 {

  case class Point(position: Pos, velocity: Pos) {
    def step: Point = Point(position + velocity, velocity)
    def step(t: Int): Point = Point(position + t *: velocity, velocity)
  }

  def boundingArea(positions: Seq[Pos]): Long = {
    val Box(min, max) = Box.bounding(positions)
    val delta = max - min
    val area = delta.x.toLong * delta.y.toLong
    area
  }

  def boundingAreaPoints(points: Seq[Point]): Long = boundingArea(points.map(_.position))

  trait Solution {
    def minimizePointsArea(points: Seq[Point]): (Seq[Point], Int)
  }

  object NaiveSolution extends Solution {

    implicit class StreamUnfoldOps(stream: Stream.type) {
      // https://github.com/tpolecat/examples/blob/ab444af9101b9049d6bd7ebf13ae583bc77ac60a/src/main/scala/eg/Unfold.scala
      def unfold[A, B](a: A)(f: A => Option[(A, B)]): Stream[B] =
        f(a).map { case (a, b) => b #:: unfold(a)(f) }.getOrElse(Stream.empty)

      def unfold0[A](a: A)(f: A => Option[A]): Stream[A] =
        unfold(a)(a => f(a).map(a => (a, a)))
    }

    override def minimizePointsArea(points: Seq[Point]): (Seq[Point], Int) = {
      val steps = Stream.unfold0((points, boundingAreaPoints(points)))({ case (points, area) =>
        val newPoints = points.map(_.step)
        val newArea = boundingAreaPoints(newPoints)

        if (newArea < area)
          Some((newPoints, newArea))
        else
          None
      }).map(_._1).toIterator // iterator is much faster because it doesn't keep old states in memory

      val (minPoints, minSecond) = steps.zipWithIndex.minBy({ case (points, second) =>
        boundingAreaPoints(points)
      })

      (minPoints, 1 + minSecond) // unfold doesn't add initial, account for it here
    }
  }

  object BinarySearchSolution extends Solution {

    def stepBoundingArea(points: Seq[Point], t: Int): Long = boundingAreaPoints(points.map(_.step(t)))

    override def minimizePointsArea(points: Seq[Point]): (Seq[Point], Int) = {
      def slope(t: Int): Long = stepBoundingArea(points, t + 1) - stepBoundingArea(points, t)

      def search(min: Int, max: Int): Int = {
        if (min == max)
          return min

        val mid = (min + max) / 2
        if (slope(mid) > 0)
          search(min, mid)
        else
          search(mid + 1, max)
      }

      def searchBounds(max: Int = 1): (Int, Int) = {
        val max2 = 2 * max
        if (slope(max2) > 0)
          (max, max2)
        else
          searchBounds(max2)
      }

      val (min, max) = searchBounds()
      val minSecond = search(min, max)
      (points.map(_.step(minSecond)), minSecond)
    }
  }

  def printPoints(points: Seq[Point]): Unit = {
    val positions = points.map(_.position)
    val Box(min, max) = Box.bounding(positions)
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
    import BinarySearchSolution._

    val (minPoints, minSecond) = minimizePointsArea(parsePoints(input))
    printPoints(minPoints)
    println(minSecond)
  }
}
