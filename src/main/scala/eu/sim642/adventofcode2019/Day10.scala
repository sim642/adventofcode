package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.{Grid, NumberTheory}
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.FractionalImplicits._

import scala.annotation.tailrec

object Day10 {

  def filterVisible(monitoring: Pos, asteroids: Set[Pos]): Set[Pos] = {

    @tailrec
    def helper(visible: Set[Pos], visibleMinDelta: Set[Pos], todo: List[Pos]): Set[Pos] = todo match {
      case Nil => visible
      case asteroid :: otherAsteroids =>
        val asteroidDelta = asteroid - monitoring
        val asteroidGcd = NumberTheory.gcd(asteroidDelta.x, asteroidDelta.y).abs
        val asteroidMinDelta = Pos(asteroidDelta.x / asteroidGcd, asteroidDelta.y / asteroidGcd)

        if (!visibleMinDelta.contains(asteroidMinDelta))
          helper(visible + asteroid, visibleMinDelta + asteroidMinDelta, otherAsteroids)
        else
          helper(visible, visibleMinDelta, otherAsteroids)
    }

    val todo = asteroids.toList.sortBy(monitoring manhattanDistance _)
    helper(Set.empty, Set.empty, todo)
  }

  def bestMonitoringPosCount(asteroids: Set[Pos]): (Pos, Int) = {
    (for {
      monitoring <- asteroids.iterator
      otherAsteroids = asteroids - monitoring
    } yield monitoring -> filterVisible(monitoring, otherAsteroids).size).maxBy(_._2)
  }

  def bestMonitoringCount(asteroids: Set[Pos]): Int = bestMonitoringPosCount(asteroids)._2

  private val halfPi = math.Pi / 2

  def laserAngle(monitoring: Pos, asteroid: Pos): Double = {
    val delta = asteroid - monitoring

    // naive
    /*val angle = math.atan2(-delta.y, delta.x) // our y-axis is flipped
    if (angle > halfPi) // (π/2, π)
      //4 * halfPi - (angle - halfPi)
      halfPi - angle + 2 * math.Pi
    else // [0, π/2] or (-π, 0)
      halfPi - angle*/

    // mod 2π
    /*val angle = math.atan2(-delta.y, delta.x) // our y-axis is flipped
    (halfPi - angle) %+ (2 * math.Pi)*/

    // atan2 abuse
    val angle = math.atan2(delta.x, delta.y) // our y-axis is flipped, atan2 axes transposed
    -angle // y-axis offset by π so no % required
  }

  def vaporizeSeq(monitoring: Pos, asteroids: Set[Pos]): Seq[Pos] = {
    if (asteroids.isEmpty)
      Seq.empty
    else {
      val visible = filterVisible(monitoring, asteroids)
      val vaporizeRotation = visible.toSeq.sortBy(laserAngle(monitoring, _))(Ordering.Double.TotalOrdering)
      vaporizeRotation ++ vaporizeSeq(monitoring, asteroids -- visible)
    }
  }

  def vaporizeBet(asteroids: Set[Pos]): Int = {
    val monitoring = bestMonitoringPosCount(asteroids)._1
    val pos = vaporizeSeq(monitoring, asteroids - monitoring)(199)
    100 * pos.x + pos.y
  }

  def parseGrid(input: String): Grid[Char] = input.linesIterator.map(_.toVector).toVector

  def parseAsteroids(input: String): Set[Pos] = {
    (for {
      (row, y) <- parseGrid(input).view.zipWithIndex
      (cell, x) <- row.view.zipWithIndex
      if cell == '#'
      pos = Pos(x, y)
    } yield pos).toSet
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bestMonitoringCount(parseAsteroids(input)))
    println(vaporizeBet(parseAsteroids(input)))
  }
}
