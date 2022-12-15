package eu.sim642.adventofcode2022

import eu.sim642.adventofcode2022.Day4.Interval
import eu.sim642.adventofcodelib.pos.Pos

import scala.annotation.tailrec
import scala.util.control.NonLocalReturns._


object Day15 {

  extension (interval: Interval) {
    def size: Int = interval.max - interval.min + 1
  }

  case class SensorBeacon(sensor: Pos, beacon: Pos) {

    val distance: Int = sensor manhattanDistance beacon

    def projectY(y: Int): Option[Interval] = {
      val dy = (y - sensor.y).abs
      val dx = distance - dy
      if (dx >= 0) {
        val left = Pos(sensor.x - dx, y)
        val right = Pos(sensor.x + dx, y)
        val left2 = if (left == beacon) left.x + 1 else left.x
        val right2 = if (right == beacon) right.x - 1 else right.x
        if (left2 <= right2)
          Some(Interval(left2, right2))
        else
          None
      }
      else
        None
    }
  }

  // Copied from 2016 day 20
  def mergeIntervals(intervals: Seq[Interval]): Seq[Interval] = {
    val sortedIntervals = intervals.sortBy(_.min)
    sortedIntervals.foldLeft(List.empty[Interval]) {
      case (acc@prev :: tl, cur) if cur.min <= prev.max + 1 =>
        /*if (cur.max <= prev.max)
          acc
        else
          Interval(prev.min, cur.max) :: tl*/
        Interval(prev.min, prev.max max cur.max) :: tl
      case (acc, cur) =>
        cur :: acc
    }
  }

  def countNoBeaconY(sensorBeacons: Seq[SensorBeacon], y: Int = 2000000): Int = {
    val intervals = sensorBeacons.flatMap(_.projectY(y))
    val mergedIntervals = mergeIntervals(intervals)
    mergedIntervals.map(_.size).sum
  }

  def findDistressBeacon(sensorBeacons: Seq[SensorBeacon], maxCoord: Int = 4000000): Pos = returning {
    val beacons = sensorBeacons.map(_.beacon).toSet

    // TODO: optimize
    for (y <- 0 to maxCoord) {
      val intervals = sensorBeacons.flatMap(_.projectY(y))
      val mergedIntervals = mergeIntervals(intervals)
      mergedIntervals match {
        case Seq(_) =>
        case Seq(right, left) =>
          val x = left.max + 1
          val pos = Pos(x, y)
          if (0 <= x && x <= maxCoord && !beacons.contains(pos))
            throwReturn(pos)
        case _ => ???
      }
    }

    ???
  }

  def tuningFrequency(sensorBeacons: Seq[SensorBeacon], maxCoord: Int = 4000000): Long = {
    val distressBeacon = findDistressBeacon(sensorBeacons, maxCoord)
    distressBeacon.x * 4000000L + distressBeacon.y
  }


  def parseSensorBeacon(s: String): SensorBeacon = s match {
    case s"Sensor at x=$sensorX, y=$sensorY: closest beacon is at x=$beaconX, y=$beaconY" =>
      SensorBeacon(Pos(sensorX.toInt, sensorY.toInt), Pos(beaconX.toInt, beaconY.toInt))
  }

  def parseSensorBeacons(input: String): Seq[SensorBeacon] = input.linesIterator.map(parseSensorBeacon).toSeq

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day15.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countNoBeaconY(parseSensorBeacons(input)))
    println(tuningFrequency(parseSensorBeacons(input)))
  }
}
