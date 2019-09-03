package eu.sim642.adventofcode2015

import Integral.Implicits._

object Day14 {

  case class Reindeer(flySpeed: Int, flyTime: Int, restTime: Int) {
    def distanceAfter(time: Int): Int = {
      val (q, r) = time /% (flyTime + restTime)
      flySpeed * (q * flyTime + (r min flyTime))
    }
  }

  def winningDistance(reindeers: Seq[Reindeer], time: Int = 2503): Int = {
    reindeers.map(_.distanceAfter(time)).max
  }

  def winningDistance(input: String): Int = winningDistance(parseInput(input))


  private val reindeerRegex = """(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.""".r

  def parseReindeer(s: String): Reindeer = s match {
    case reindeerRegex(name, flySpeed, flyTime, restTime) =>
      Reindeer(flySpeed.toInt, flyTime.toInt, restTime.toInt)
  }

  def parseInput(input: String): Seq[Reindeer] = input.linesIterator.map(parseReindeer).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(winningDistance(input))
  }
}
