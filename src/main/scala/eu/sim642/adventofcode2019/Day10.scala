package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.{Grid, NumberTheory}
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec

object Day10 {

  def isBlocked(monitoring: Pos, blocker: Pos, asteroid: Pos): Boolean = {
    val blockerDelta = blocker - monitoring
    val blockerGcd = NumberTheory.gcd(blockerDelta.x, blockerDelta.y).abs
    val blockerMinDelta = Pos(blockerDelta.x / blockerGcd, blockerDelta.y / blockerGcd)

    val asteroidDelta = asteroid - monitoring

    //val crossZ = blockerDelta.x * asteroidDelta.y - blockerDelta.y * asteroidDelta.x

    if (blockerMinDelta.x * asteroidDelta.y == asteroidDelta.x * blockerMinDelta.y) {
      if (blockerMinDelta.x != 0)
        asteroidDelta.x / blockerMinDelta.x > 1
      else
        asteroidDelta.y / blockerMinDelta.y > 1
    }
    else
      false
  }

  def filterVisible(monitoring: Pos, asteroids: Set[Pos]): Set[Pos] = {

    @tailrec
    def helper(visible: Set[Pos], todo: List[Pos]): Set[Pos] = todo match {
      case Nil => visible
      case asteroid :: otherAsteroids =>
        if (visible.forall(!isBlocked(monitoring, _, asteroid)))
          helper(visible + asteroid, otherAsteroids)
        else
          helper(visible, otherAsteroids)
    }

    val todo = asteroids.toList.sortBy(monitoring manhattanDistance _)
    helper(Set.empty, todo)
  }

  def bestMonitoringPosCount(asteroids: Set[Pos]): (Pos, Int) = {
    (for {
      monitoring <- asteroids.iterator
      otherAsteroids = asteroids - monitoring
    } yield monitoring -> filterVisible(monitoring, otherAsteroids).size).maxBy(_._2)
  }

  def bestMonitoringCount(asteroids: Set[Pos]): Int = bestMonitoringPosCount(asteroids)._2

  def laserAngle(monitoring: Pos, asteroid: Pos): Double = {
    val delta = asteroid - monitoring
    val angle = math.atan2(-delta.y, delta.x)
    val angle2 = if (angle > math.Pi / 2) angle - 2 * math.Pi else angle
    math.Pi / 2 - angle2
  }

  def vaporizeSeq(monitoring: Pos, asteroids: Set[Pos]): Seq[Pos] = {
    if (asteroids.isEmpty)
      Seq.empty
    else {
      val visible = filterVisible(monitoring, asteroids)
      val vaporizeRotation = visible.toSeq.sortBy(laserAngle(monitoring, _))
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
    } yield Pos(x, y)).toSet
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day10.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(bestMonitoringCount(parseAsteroids(input)))
    println(vaporizeBet(parseAsteroids(input)))
  }
}
