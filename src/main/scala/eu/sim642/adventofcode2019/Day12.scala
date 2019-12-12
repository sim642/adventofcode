package eu.sim642.adventofcode2019

import eu.sim642.adventofcodelib.pos.Pos3
import eu.sim642.adventofcodelib.IteratorImplicits._
import eu.sim642.adventofcodelib.NumberTheory
import eu.sim642.adventofcodelib.cycle.{BrentCycleFinder, NaiveCycleFinder}

object Day12 {

  case class Moon(pos: Pos3, vel: Pos3) {
    def applyGravity(moons: Seq[Moon]): Moon = {
      def calculateGravity(moon: Moon): Pos3 = {
        def axisGravity(myVal: Int, moonVal: Int): Int = {
          if (myVal == moonVal)
            0
          else if (myVal < moonVal)
            1
          else
            -1
        }

        Pos3(
          axisGravity(pos.x, moon.pos.x),
          axisGravity(pos.y, moon.pos.y),
          axisGravity(pos.z, moon.pos.z)
        )
      }

      val totalGravity = moons.view.map(calculateGravity).reduce(_ + _)
      Moon(pos, vel + totalGravity)
    }

    def applyVelocity: Moon = Moon(pos + vel, vel)

    def potentialEnergy: Int = pos.x.abs + pos.y.abs + pos.z.abs
    def kineticEnergy: Int = vel.x.abs + vel.y.abs + vel.z.abs
    def energy: Int = potentialEnergy * kineticEnergy
  }

  def stepMoons(moons: Seq[Moon]): Seq[Moon] = {
    moons.map(_.applyGravity(moons)).map(_.applyVelocity)
  }

  def iterateMoons(moons: Seq[Moon]): Iterator[Seq[Moon]] = Iterator.iterate(moons)(stepMoons)

  def totalEnergy(moons: Seq[Moon]): Int = moons.view.map(_.energy).sum

  def simulateTotalEnergy(moons: Seq[Moon], steps: Int = 1000): Int = {
    val finalMoons = iterateMoons(moons)(steps)
    totalEnergy(finalMoons)
  }

  trait Part2Solution {
    def simulateCycleSteps(moons: Seq[Moon]): Long
  }

  /**
    * Part 2 solution which naïvely simulates until cycle is found.
    */
  object NaivePart2Solution extends Part2Solution {
    override def simulateCycleSteps(moons: Seq[Moon]): Long = {
      NaiveCycleFinder.find(moons, stepMoons).stemCycleLength
      //BrentCycleFinder.find(moons, stepMoons).stemCycleLength
    }
  }

  def xMoons(moons: Seq[Moon]): Seq[(Int, Int)] = moons.map(moon => (moon.pos.x, moon.vel.x))
  def yMoons(moons: Seq[Moon]): Seq[(Int, Int)] = moons.map(moon => (moon.pos.y, moon.vel.y))
  def zMoons(moons: Seq[Moon]): Seq[(Int, Int)] = moons.map(moon => (moon.pos.z, moon.vel.z))

  /**
    * Part 2 solution which finds per-axis cycles with the first state repeating.
    * The first state will be the one to repeat since a step is invertible.
    * Overall cycle step count is the LCM of the per-axis cycle lengths.
    */
  object LcmPart2Solution extends Part2Solution {
    override def simulateCycleSteps(moons: Seq[Moon]): Long = {
      val iterateMoons = LazyList.iterate(moons)(stepMoons)

      def cycleLength[A](f: Seq[Moon] => A): Int = {
        iterateMoons.view.map(f).indexOf(f(moons), 1)
      }

      val xCycleLength = cycleLength(xMoons)
      val yCycleLength = cycleLength(yMoons)
      val zCycleLength = cycleLength(zMoons)

      def lcm(a: Long, b: Long): Long = a * b / NumberTheory.gcd(a.toInt, b.toInt) // TODO: no toInt

      // TODO: multi-way LCM
      lcm(lcm(xCycleLength, yCycleLength), zCycleLength)
    }
  }

  /**
    * Part 2 solution which finds per-axis cycles in general.
    * Overall cycle step count is calculated by CRT of the per-axis cycles.
    */
  object CrtPart2Solution extends Part2Solution {
    override def simulateCycleSteps(moons: Seq[Moon]): Long = {
      val cycleFinder = NaiveCycleFinder.findBy(moons, stepMoons) _
      val xCycle = cycleFinder(xMoons)
      val yCycle = cycleFinder(yMoons)
      val zCycle = cycleFinder(zMoons)

      // TODO: remove implicit assumption using CRT
      assert(xCycle.stemLength == 0)
      assert(yCycle.stemLength == 0)
      assert(zCycle.stemLength == 0)

      def lcm(a: Long, b: Long): Long = a * b / NumberTheory.gcd(a.toInt, b.toInt) // TODO: no toInt

      lcm(lcm(xCycle.cycleLength, yCycle.cycleLength), zCycle.cycleLength)
    }
  }

  private val moonRegex = """<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>""".r

  def parseMoon(s: String): Moon = s match {
    case moonRegex(x, y, z) =>
      Moon(Pos3(x.toInt, y.toInt, z.toInt), Pos3.zero)
  }

  def parseMoons(input: String): Seq[Moon] = input.linesIterator.map(parseMoon).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import CrtPart2Solution._

    println(simulateTotalEnergy(parseMoons(input)))
    println(simulateCycleSteps(parseMoons(input)))
  }
}
