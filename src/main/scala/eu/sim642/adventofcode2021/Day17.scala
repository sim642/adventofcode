package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos

object Day17 {

  def simulateY(initialYVelocity: Int): Iterator[Int] = {
    Iterator.iterate((0, initialYVelocity))((y, yVelocity) => (y + yVelocity, yVelocity - 1)).map(_._1)
  }

  def hitsTargetY(target: Box, initialYVelocity: Int): Boolean = {
    simulateY(initialYVelocity).takeWhile(_ >= target.min.y).exists(_ <= target.max.y)
  }

  def initialVelocityBounds(target: Box): Box = {
    Box(Pos(0, target.min.y), Pos(target.max.x, target.min.y.abs))
  }

  def findHighestY(target: Box): Int = {
    val bounds = initialVelocityBounds(target)
    (bounds.min.y to bounds.max.y)
      .filter(hitsTargetY(target, _))
      .map(initialYVelocity => simulateY(initialYVelocity).takeWhile(_ >= target.min.y).max)
      .max
  }

  def simulateX(initialXVelocity: Int): Iterator[Int] = {
    Iterator.iterate((0, initialXVelocity))((x, xVelocity) => (x + xVelocity, xVelocity + (if (xVelocity > 0) -1 else 0))).map(_._1)
  }

  def hitsTargetX(target: Box, initialXVelocity: Int): Boolean = {
    simulateY(initialXVelocity).takeWhile(_ <= target.max.x).exists(_ >= target.min.x)
  }

  def simulate(initialVelocity: Pos): Iterator[Pos] = {
    (simulateX(initialVelocity.x) zip simulateY(initialVelocity.y)).map(Pos.apply)
  }

  def hitsTarget(target: Box, initialVelocity: Pos): Boolean = {
    simulate(initialVelocity).takeWhile(pos => pos.x <= target.max.x && pos.y >= target.min.y).exists(target.contains)
  }

  def countHitsTarget(target: Box): Int = {
    val bounds = initialVelocityBounds(target)
    val ys = (bounds.min.y to bounds.max.y).filter(hitsTargetY(target, _))
    val xs = (bounds.min.x to bounds.max.x).filter(hitsTargetX(target, _))
    val initials = for {
      x <- xs
      y <- ys
      pos = Pos(x, y)
      if hitsTarget(target, pos)
    } yield pos
    initials.size
  }


  private val targetRegex = """target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)""".r

  def parseTarget(input: String): Box = input match {
    case targetRegex(xMin, xMax, yMin, yMax) =>
      Box(Pos(xMin.toInt, yMin.toInt), Pos(xMax.toInt, yMax.toInt))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(findHighestY(parseTarget(input)))
    println(countHitsTarget(parseTarget(input)))

    // part 2: 369 - wrong (x range -100 to 100 instead of 0 to 1000)
  }
}
