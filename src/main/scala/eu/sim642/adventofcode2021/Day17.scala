package eu.sim642.adventofcode2021

import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits._

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
    Iterator.iterate((0, initialXVelocity))((x, xVelocity) => (x + xVelocity, xVelocity - xVelocity.sign)).map(_._1)
  }

  def hitsTargetX(target: Box, initialXVelocity: Int): Boolean = {
    simulateX(initialXVelocity).takeWhile(_ <= target.max.x).exists(_ >= target.min.x)
  }

  def simulate(initialVelocity: Pos): Iterator[Pos] = {
    (simulateX(initialVelocity.x) zip simulateY(initialVelocity.y)).map(Pos.apply)
  }

  def hitsTarget(target: Box, initialVelocity: Pos): Boolean = {
    simulate(initialVelocity).takeWhile(pos => pos.x <= target.max.x && pos.y >= target.min.y).exists(target.contains)
  }

  sealed trait Part2Solution {
    def iterateHitsTarget(target: Box): IterableOnce[Pos]

    def countHitsTarget(target: Box): Int = iterateHitsTarget(target).iterator.size
  }

  object SimulatePart2Solution extends Part2Solution {
    override def iterateHitsTarget(target: Box): IterableOnce[Pos] = {
      val bounds = initialVelocityBounds(target)
      val ys = (bounds.min.y to bounds.max.y).filter(hitsTargetY(target, _))
      //val xs = (bounds.min.x to bounds.max.x).filter(hitsTargetX(target, _))
      val xs = bounds.min.x to bounds.max.x
      for {
        x <- xs.iterator
        y <- ys.iterator
        pos = Pos(x, y)
        if hitsTarget(target, pos)
      } yield pos
    }
  }

  object AxisTimePart2Solution extends Part2Solution {

    def hitsTargetYTs(target: Box, initialYVelocity: Int): Seq[Int] = {
      simulateY(initialYVelocity).takeWhile(_ >= target.min.y).zipWithIndex.filter(_._1 <= target.max.y).map(_._2).toSeq
    }

    def hitsTargetXTs(target: Box, initialXVelocity: Int, maxT: Int): Seq[Int] = {
      simulateX(initialXVelocity).takeWhile(_ <= target.max.x).zipWithIndex.takeWhile(_._2 <= maxT).filter(_._1 >= target.min.x).map(_._2).toSeq
    }

    override def iterateHitsTarget(target: Box): IterableOnce[Pos] = {
      val bounds = initialVelocityBounds(target)
      val ys = (for {
        y <- (bounds.min.y to bounds.max.y).iterator
        t <- hitsTargetYTs(target, y)
      } yield t -> y).groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
      val maxT = ys.keys.max
      val xs = (for {
        x <- (bounds.min.x to bounds.max.x).iterator
        t <- hitsTargetXTs(target, x, maxT)
      } yield t -> x).groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
      (for {
        (t, yc) <- ys.iterator
        xc <- xs.get(t).iterator
        x <- xc
        y <- yc
      } yield Pos(x, y)).toSet // need toSet to deduplicate those which stay in target for more than one timestamp
    }
  }


  private val targetRegex = """target area: x=(\d+)..(\d+), y=(-?\d+)..(-?\d+)""".r

  def parseTarget(input: String): Box = input match {
    case targetRegex(xMin, xMax, yMin, yMax) =>
      Box(Pos(xMin.toInt, yMin.toInt), Pos(xMax.toInt, yMax.toInt))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day17.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import AxisTimePart2Solution._

    println(findHighestY(parseTarget(input)))
    println(countHitsTarget(parseTarget(input)))

    // part 2: 369 - wrong (x range -100 to 100 instead of 0 to 1000)
  }
}
