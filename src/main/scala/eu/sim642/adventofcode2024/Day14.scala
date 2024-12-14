package eu.sim642.adventofcode2024

import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.IterableImplicits._

object Day14 {

  // copied from 2021 day 25
  extension (pos: Pos) {
    def %+(other: Pos): Pos = Pos(pos.x %+ other.x, pos.y %+ other.y)
    infix def compareTo(other: Pos): Pos = Pos(pos.x `compareTo` other.x, pos.y `compareTo` other.y)
    def sign: Pos = Pos(pos.x.sign, pos.y.sign)
  }

  case class Robot(pos: Pos, velocity: Pos) {
    def step(roomSize: Pos): Robot = Robot((pos + velocity) %+ roomSize, velocity)
    def step(t: Int, roomSize: Pos): Robot = Robot((pos + t *: velocity) %+ roomSize, velocity)
  }

  val inputRoomSize: Pos = Pos(101, 103)

  def safetyFactor(robots: Seq[Robot], roomSize: Pos = inputRoomSize): Int = {
    val newRobots = robots.map(_.step(100, roomSize))
    val roomMiddle = Pos(roomSize.x / 2, roomSize.y / 2)
    val quadrants = newRobots.groupCount(robot =>
      (robot.pos `compareTo` roomMiddle).sign // use .sign to clamp to -1, 0, 1
    ).withDefaultValue(0)
    Pos.diagonalOffsets.map(quadrants).product
  }

  def findEasterEgg(robots: Seq[Robot], roomSize: Pos = inputRoomSize): (Seq[Robot], Int) = {
    Iterator.iterate(robots)(_.map(_.step(roomSize)))
      .zipWithIndex
      .find(_._1.groupCount(_.pos).values.forall(_ <= 1)) // look for arrangement with unique positions
      // TODO: optimize by stopping groupCount when some goes over 1
      .get
  }

  def printRobots(robots: Seq[Robot], roomSize: Pos = inputRoomSize): Unit = {
    val posRobots = robots.groupCount(_.pos)

    for (y <- 0 until roomSize.y) {
      for (x <- 0 until roomSize.x) {
        val pos = Pos(x, y)
        print(posRobots.get(pos).map(count => ('0' + count).toChar).getOrElse('.')) // TODO: toDigit?
      }
      println()
    }
  }

  def parseRobot(s: String): Robot = s match {
    case s"p=$pX,$pY v=$vX,$vY" =>
      Robot(Pos(pX.toInt, pY.toInt), Pos(vX.toInt, vY.toInt))
  }

  def parseRobots(input: String): Seq[Robot] = input.linesIterator.map(parseRobot).toSeq

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day14.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(safetyFactor(parseRobots(input)))

    val (easterEggRobots, easterEggTime) = findEasterEgg(parseRobots(input))
    printRobots(easterEggRobots)
    println(easterEggTime)
  }
}
