package eu.sim642.adventofcode2025

import scala.collection.mutable

object Day11 {

  type Device = String

  trait Part {
    val from: Device
    val via: Set[Device]
    val to: Device = "out"

    def countPaths(devices: Map[Device, Seq[Device]]): Long = {
      val memo = mutable.Map.empty[Device, Map[Set[Device], Long]]

      def helper(device: Device): Map[Set[Device], Long] = {
        memo.getOrElseUpdate(device, {
          val deviceVia = via.intersect(Set(device))
          if (device == to)
            Map(deviceVia -> 1)
          else
            devices(device).flatMap(helper).groupMapReduce(_._1 ++ deviceVia)(_._2)(_ + _)
        })
      }

      helper(from)(via)
    }
  }

  object Part1 extends Part {
    override val from: Device = "you"
    override val via: Set[Device] = Set.empty
  }

  object Part2 extends Part {
    override val from: Device = "svr"
    override val via: Set[Device] = Set("dac", "fft")
  }

  def parseDevice(s: String): (Device, Seq[Device]) = s match {
    case s"$key: $values" => key -> values.split(" ").toSeq
  }

  def parseDevices(input: String): Map[Device, Seq[Device]] = input.linesIterator.map(parseDevice).toMap

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.countPaths(parseDevices(input)))
    println(Part2.countPaths(parseDevices(input)))
  }
}
