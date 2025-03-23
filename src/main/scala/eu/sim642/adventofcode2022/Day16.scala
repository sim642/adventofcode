package eu.sim642.adventofcode2022

import eu.sim642.adventofcodelib.IteratorImplicits.GroupIteratorOps

import scala.annotation.tailrec
import scala.collection.immutable.BitSet
import scala.collection.mutable

object Day16 {

  type Valve = String

  case class ValveData(flowRate: Int, tunnels: Seq[Valve])

  // https://en.wikipedia.org/wiki/Transit_node_routing
  def valveDists(valves: Map[Valve, ValveData]): collection.Map[Valve, collection.Map[Valve, Int]] = {
    val dists: mutable.Map[Valve, mutable.Map[Valve, Int]] = valves.view.mapValues(_.tunnels.map(_ -> 1).to(mutable.Map)).to(mutable.Map)
    // Floyd-Warshall
    // TODO: extract Floyd-Warshall to library
    for {
      midValve <- valves.keys
      fromValve <- valves.keys
      fromDist <- dists(fromValve).get(midValve)
      toValve <- valves.keys
      toDist <- dists(midValve).get(toValve)
      newDist = fromDist + toDist
      if dists(fromValve).get(toValve).forall(_ > newDist)
    } dists(fromValve)(toValve) = newDist
    dists
  }

  def valvesMaxPressure(valves: Map[Valve, ValveData], minutes: Int): Map[BitSet, Int] = {
    val dists = valveDists(valves) // precompute pairwise distances between valves for direct moves to good valves

    // fix valve order to use BitSet, which is much faster for part 2
    val orderedValves = valves.keys.toVector
    val goodValveIndices = valves.filter(_._2.flowRate > 0).keySet.map(orderedValves.indexOf)

    case class State(valve: Valve, open: BitSet)

    @tailrec
    def helper(minute: Int, states: Map[Int, Map[State, Int]]): Map[BitSet, Int] = {
      if (minute < minutes) {
        val moveOpenStates = for {
          (State(valve, open), pressure) <- states.getOrElse(minute, Map.empty).iterator
          newValveIndex <- goodValveIndices // only go to good valves
          if !open.contains(newValveIndex)
          newValve = orderedValves(newValveIndex)
          dist = dists(valve)(newValve)
          flowRate = valves(newValve).flowRate // also open good valve, otherwise pointless move
          doneMinute = minute + dist + 1
          if doneMinute < minutes // avoid move, which won't finish in time
          extraPressure = flowRate * (minutes - doneMinute)
        } yield doneMinute -> (State(newValve, open + newValveIndex) -> (pressure + extraPressure))

        // TODO: better nested merging
        val newStates = moveOpenStates.foldLeft(states)({ case (acc, (minute, (state, pressure))) =>
          val minuteStates = acc.getOrElse(minute, Map.empty)
          val newPressure = minuteStates.getOrElse(state, 0) max pressure
          val newMinuteStates = minuteStates.updated(state, newPressure)
          acc.updated(minute, newMinuteStates)
        })

        helper(minute + 1, newStates)
      }
      else {
        (for {
          minuteStates <- states.valuesIterator
          (state, pressure) <- minuteStates.iterator
        } yield state.open -> pressure).groupMapReduce(_._1)(_._2)(_ max _)
      }
    }

    helper(0, Map(0 -> Map(State("AA", BitSet.empty) -> 0)))
  }

  trait Part {
    def mostPressure(valves: Map[Valve, ValveData]): Int
  }

  object Part1 extends Part {
    override def mostPressure(valves: Map[Valve, ValveData]): Int = {
      valvesMaxPressure(valves, 30).values.max
    }
  }

  object Part2 extends Part {
    override def mostPressure(valves: Map[Valve, ValveData]): Int = {
      val valvesPressure = valvesMaxPressure(valves, 26).toVector
      (for {
        ((valves1, pressure1), i) <- valvesPressure.iterator.zipWithIndex
        (valves2, pressure2) <- valvesPressure.view.drop(i + 1).iterator
        if (valves1 intersect valves2).isEmpty
      } yield pressure1 + pressure2).max
    }
  }


  def parseValve(s: String): (Valve, ValveData) = s match {
    case s"Valve $valve has flow rate=$flowRate; tunnel$_ lead$_ to valve$_ $tunnels" =>
      valve -> ValveData(flowRate.toInt, tunnels.split(", ").toSeq)
  }

  def parseValves(input: String): Map[Valve, ValveData] = input.linesIterator.map(parseValve).toMap


  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(Part1.mostPressure(parseValves(input)))
    println(Part2.mostPressure(parseValves(input)))
  }
}
