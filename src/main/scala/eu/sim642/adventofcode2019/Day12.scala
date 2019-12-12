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

  def simulateCycleSteps(moons: Seq[Moon]): Long = {
    //NaiveCycleFinder.find(moons, stepMoons).stemCycleLength
    //BrentCycleFinder.find(moons, stepMoons).stemCycleLength

    // TODO: separate this to Part2Solutions
    val cycleFinder = NaiveCycleFinder.findBy(moons, stepMoons) _
    val xCycle = cycleFinder(_.map(moon => (moon.pos.x, moon.vel.x)))
    val yCycle = cycleFinder(_.map(moon => (moon.pos.y, moon.vel.y)))
    val zCycle = cycleFinder(_.map(moon => (moon.pos.z, moon.vel.z)))

    // TODO: remove implicit assumption using CRT
    assert(xCycle.stemLength == 0)
    assert(yCycle.stemLength == 0)
    assert(zCycle.stemLength == 0)

    def lcm(a: Long, b: Long): Long = a * b / NumberTheory.gcd(a.toInt, b.toInt) // TODO: no toInt

    lcm(lcm(xCycle.cycleLength, yCycle.cycleLength), zCycle.cycleLength)
  }


  private val moonRegex = """<x=(-?\d+),\s*y=(-?\d+),\s*z=(-?\d+)>""".r

  def parseMoon(s: String): Moon = s match {
    case moonRegex(x, y, z) =>
      Moon(Pos3(x.toInt, y.toInt, z.toInt), Pos3.zero)
  }

  def parseMoons(input: String): Seq[Moon] = input.linesIterator.map(parseMoon).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day12.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(simulateTotalEnergy(parseMoons(input)))
    println(simulateCycleSteps(parseMoons(input)))
  }
}
