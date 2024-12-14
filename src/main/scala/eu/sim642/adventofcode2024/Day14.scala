package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._

object Day14 {

  // copied from 2021 day 25
  extension (pos: Pos) {
    def %+(other: Pos): Pos = Pos(pos.x %+ other.x, pos.y %+ other.y)
  }

  case class Robot(pos: Pos, velocity: Pos) {
    def step(t: Int, roomSize: Pos): Robot = Robot((pos + t *: velocity) %+ roomSize, velocity)
  }

  def safetyFactor(robots: Seq[Robot], roomSize: Pos = Pos(101, 103)): Int = {
    val newRobots = robots.map(_.step(100, roomSize))
    //newRobots.foreach(println)
    val roomMiddle = Pos(roomSize.x / 2, roomSize.y / 2)
    //println(roomMiddle)
    //val quadrants0 = newRobots.groupBy(robot => (robot.pos.x.compare(roomMiddle.x), robot.pos.y.compare(roomMiddle.y))).withDefaultValue(0)
    //quadrants0.foreach(println)
    val quadrants = newRobots.groupCount(robot => (robot.pos.x.compare(roomMiddle.x), robot.pos.y.compare(roomMiddle.y))).withDefaultValue(0)
    quadrants((1, 1)) * quadrants((-1, 1)) * quadrants((1, -1)) * quadrants((-1, -1))
  }

  def parseRobot(s: String): Robot = s match {
    case s"p=$pX,$pY v=$vX,$vY" =>
      Robot(Pos(pX.toInt, pY.toInt), Pos(vX.toInt, vY.toInt))
  }

  def parseRobots(input: String): Seq[Robot] = input.linesIterator.map(parseRobot).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(safetyFactor(parseRobots(input)))
  }
}
