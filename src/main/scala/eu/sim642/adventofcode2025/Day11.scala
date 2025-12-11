package eu.sim642.adventofcode2025

import scala.collection.mutable

object Day11 {

  type Device = String

  def countPaths(devices: Map[Device, Seq[Device]], from: Device = "you", to: Device = "out"): Int = { // TODO: Long?

    val memo = mutable.Map.empty[Device, Int]

    def helper(device: Device): Int = {
      memo.getOrElseUpdate(device, {
        if (device == to)
          1
        else
          devices(device).map(helper).sum
      })
    }

    helper(from)
  }

  def parseDevice(s: String): (Device, Seq[Device]) = s match {
    case s"$key: $values" => key -> values.split(" ").toSeq
  }

  def parseDevices(input: String): Map[Device, Seq[Device]] = input.linesIterator.map(parseDevice).toMap

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPaths(parseDevices(input)))
  }
}
