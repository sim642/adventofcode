package eu.sim642.adventofcode2025

import scala.collection.mutable

object Day11 {

  type Device = String
  type Devices = Map[Device, Seq[Device]]

  trait PartDevices {
    val from: Device
    val via: Set[Device]
    val to: Device = "out"
  }

  trait Part1Devices extends PartDevices {
    override val from: Device = "you"
    override val via: Set[Device] = Set.empty
  }

  trait Part2Devices extends PartDevices {
    override val from: Device = "svr"
    override val via: Set[Device] = Set("dac", "fft")
  }

  trait PartSolution extends PartDevices {
    def countPaths(devices: Devices): Long
  }

  trait Solution {
    val Part1: PartSolution
    val Part2: PartSolution
  }

  /**
   * Solution, which counts paths separately for subsets of visited vias.
   */
  object ViaMapSolution extends Solution {
    trait ViaMapPartSolution extends PartSolution {
      override def countPaths(devices: Devices): Long = {
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

    override object Part1 extends ViaMapPartSolution with Part1Devices
    override object Part2 extends ViaMapPartSolution with Part2Devices
  }

  // TODO: path count product solution

  def parseDevice(s: String): (Device, Seq[Device]) = s match {
    case s"$key: $values" => key -> values.split(" ").toSeq
  }

  def parseDevices(input: String): Devices = input.linesIterator.map(parseDevice).toMap

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day11.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    import ViaMapSolution._

    println(Part1.countPaths(parseDevices(input)))
    println(Part2.countPaths(parseDevices(input)))
  }
}
