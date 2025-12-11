package eu.sim642.adventofcode2025

import scala.annotation.tailrec
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

  /**
   * Optimized version of [[ViaMapSolution]], which only keeps the largest key pair from the mapping.
   * Others are useless for passing all the vias.
   */
  object ViaPairSolution extends Solution {
    trait ViaPairPartSolution extends PartSolution {
      override def countPaths(devices: Devices): Long = {
        val memo = mutable.Map.empty[Device, (Set[Device], Long)]

        def helper(device: Device): (Set[Device], Long) = {
          memo.getOrElseUpdate(device, {
            val deviceVia = via.intersect(Set(device))
            if (device == to)
              deviceVia -> 1
            else {
              val (a, b) = devices(device).map(helper).foldLeft(Set.empty[Device] -> 0L)({ case (a@(accVia, acc), n@(newVia, newCount)) =>
                if (accVia == newVia)
                  accVia -> (acc + newCount)
                else if (accVia subsetOf newVia)
                  n
                else if (newVia subsetOf accVia)
                  a
                else
                  throw new IllegalStateException("")
              })
              a.union(deviceVia) -> b
            }
          })
        }

        helper(from)._2
      }
    }

    override object Part1 extends ViaPairPartSolution with Part1Devices

    override object Part2 extends ViaPairPartSolution with Part2Devices
  }

  /**
   * Solution, which tries all permutations of vias and counts each one by multiplying adjacent steps.
   */
  object PermutationSolution extends Solution {
    trait PermutationPartSolution extends PartSolution {
      def countPathsTo(devices: Devices)(to: Device): Device => Long = {
        val memo = mutable.Map.empty[Device, Long]

        def helper(device: Device): Long = {
          memo.getOrElseUpdate(device, {
            if (device == to)
              1
            else
              devices.getOrElse(device, Set.empty).map(helper).sum // need getOrElse for "out" when from is different, but reaches "out"
          })
        }

        helper
      }

      override def countPaths(devices: Devices): Long = {
        val memo = mutable.Map.empty[Device, Device => Long]

        def countPathsFromTo(from: Device, to: Device): Long = // memoize by to, because same to will be reused
          memo.getOrElseUpdate(to, countPathsTo(devices)(to))(from)

        @tailrec
        def helper(prevDevice: Device, acc: Long, via: List[Device]): Long = via match {
          case Nil => acc * countPathsFromTo(prevDevice, to)
          case device :: newVia =>
            val newAcc = countPathsFromTo(prevDevice, device)
            if (newAcc == 0)
              0 // this step is impossible, so no need to continue (multiplying 0)
            else
              helper(device, acc * newAcc, newVia)
        }

        via.toList.permutations.map(helper(from, 1L, _)).find(_ != 0).get // this could be a sum, but only one vias permutation can be non-zero, so stop after finding that one, no need to continue (adding 0s)
      }
    }

    override object Part1 extends PermutationPartSolution with Part1Devices
    override object Part2 extends PermutationPartSolution with Part2Devices
  }

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
