package eu.sim642.adventofcode2025

import scala.collection.mutable

object Day11 {

  type Device = String

  def countPaths(devices: Map[Device, Seq[Device]], from: Device = "you", to: Device = "out"): Int = {
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

  def countPaths2(devices: Map[Device, Seq[Device]], from: Device = "svr", to: Device = "out"): Long = {
    val vias = Set("dac", "fft")

    val memo = mutable.Map.empty[Device, Map[Set[Device], Long]]

    def helper(device: Device): Map[Set[Device], Long] = {
      memo.getOrElseUpdate(device, {
        if (device == to)
          Map(Set.empty -> 1)
        else
          devices(device).flatMap(helper).groupMapReduce(_._1)(_._2)(_ + _).map((k, v) => (k.union(vias.intersect(Set(device)))) -> v)
      })
    }

    helper(from)(vias)
  }

  def parseDevice(s: String): (Device, Seq[Device]) = s match {
    case s"$key: $values" => key -> values.split(" ").toSeq
  }

  def parseDevices(input: String): Map[Device, Seq[Device]] = input.linesIterator.map(parseDevice).toMap

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(countPaths(parseDevices(input)))
    println(countPaths2(parseDevices(input)))
  }
}
