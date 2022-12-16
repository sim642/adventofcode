package eu.sim642.adventofcode2022

import eu.sim642.adventofcode2022.Day4.Interval
import eu.sim642.adventofcodelib.box.Box
import eu.sim642.adventofcodelib.pos.Pos
import eu.sim642.adventofcodelib.IteratorImplicits.GroupIteratorOps

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonLocalReturns.*

object Day16 {

  type Valve = String

  case class ValveData(flowRate: Int, tunnels: Seq[Valve])

  def mostPressure(valves: Map[Valve, ValveData]): Int = {

    /*case class State(valve: Valve, open: Set[Valve], remainingFlowRate: Int)

    @tailrec
    def helper(minute: Int, states: Map[State, Int]): Int = {
      println(s"$minute: ${states.size}")
      if (minute < 30) {
        val moveStates = for {
          (state, pressure) <- states.iterator
          newValve <- valves(state.valve).tunnels
        } yield state.copy(valve = newValve) -> pressure

        val openStates = for {
          (state, pressure) <- states.iterator
          flowRate = valves(state.valve).flowRate
          if flowRate >= 0
          if !state.open.contains(state.valve)
          extraPressure = flowRate * (30 - minute - 1)
        } yield state.copy(open = state.open + state.valve, remainingFlowRate = state.remainingFlowRate - flowRate) -> (pressure + extraPressure)

        val newStates = (moveStates ++ openStates).groupMapReduce(_._1)(_._2)(_ max _)
        //println(newStates)
        val maxPressure = newStates.values.max
        //println(maxPressure)
        val newStates2 = newStates.filter(p => p._2 + p._1.remainingFlowRate * (30 - minute - 2) >= maxPressure)
        //println(newStates2)
        helper(minute + 1, newStates2)
      }
      else
        states.values.max
    }

    helper(0, Map(State("AA", Set.empty, valves.values.map(_.flowRate).sum) -> 0))*/


    val dists: mutable.Map[Valve, mutable.Map[Valve, Int]] = valves.view.mapValues(_.tunnels.map(_ -> 1).to(mutable.Map)).to(mutable.Map)
    // Floyd-Warshall
    for {
      midValve <- valves.keys
      fromValve <- valves.keys
      fromDist <- dists(fromValve).get(midValve)
      toValve <- valves.keys
      toDist <- dists(midValve).get(toValve)
      newDist = fromDist + toDist
      if dists(fromValve).get(toValve).forall(_ > newDist)
    } dists(fromValve)(toValve) = newDist

    val goodValves = valves.filter(_._2.flowRate > 0).keySet

    case class State(valve: Valve, open: Set[Valve], remainingFlowRate: Int)

    @tailrec
    def helper(minute: Int, states2: Map[Int, Map[State, Int]]): Int = {
      val states = states2.getOrElse(minute, Map.empty)
      println(s"$minute: ${states.size}")
      if (minute < 30) {
        val moveOpenStates = for {
          (state, pressure) <- states.iterator
          newValve <- goodValves
          if !state.open.contains(newValve)
          dist = dists(state.valve)(newValve)
          flowRate = valves(newValve).flowRate
          extraPressure = flowRate * (30 - minute - dist - 1)
        } yield (minute + dist + 1) -> (state.copy(valve = newValve, open = state.open + newValve, remainingFlowRate = state.remainingFlowRate - flowRate) -> (pressure + extraPressure))

        //val s2 = moveOpenStates.groupMapReduce(_._1)(p => Set(p._2))(_ ++ _)
        //val s3 = s2.view.mapValues(_.groupMapReduce(_._1)(_._2)(_ max _)).toMap
        val s4 = moveOpenStates.foldLeft(states2)({ case (acc, (min, (st, dist))) =>
          val m1 = acc.getOrElse(min, Map.empty)
          val d1 = m1.getOrElse(st, 0)
          val d2 = d1 max dist
          val m2 = m1.updated(st, d2)
          acc.updated(min, m2)
        })

        helper(minute + 1, s4)
      }
      else
        states2.flatMap(_._2.values.maxOption).max
    }

    helper(0, Map(0 -> Map(State("AA", Set.empty, valves.values.map(_.flowRate).sum) -> 0)))
  }


  def parseValve(s: String): (Valve, ValveData) = s match {
    case s"Valve $valve has flow rate=$flowRate; tunnel$_ lead$_ to valve$_ $tunnels" =>
      valve -> ValveData(flowRate.toInt, tunnels.split(", ").toSeq)
  }

  def parseValves(input: String): Map[Valve, ValveData] = input.linesIterator.map(parseValve).toMap


  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(mostPressure(parseValves(input)))
  }
}
