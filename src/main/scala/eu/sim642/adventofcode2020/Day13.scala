package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IntegralImplicits._
import eu.sim642.adventofcodelib.NumberTheory

object Day13 {

  type Buses = Seq[Option[Int]]
  case class Notes(earliestDepart: Int, buses: Buses)

  def earliestBusWaitTime(notes: Notes): Int = {
    val (earliestBus, waitTime) = notes.buses.view
      .flatten
      .map(bus => (bus, bus - notes.earliestDepart % bus)) // what about 0?
      .minBy(_._2)
    earliestBus * waitTime
  }

  def earliestSubsequentDepart(buses: Buses): BigInt = {
    // x buses can be ignored (choose bus ID to be t + i)

    // t + i = 0 (mod bus)
    // t = -i (mod bus)

    val ans = buses.view
      .zipWithIndex
      .flatMap({ case (bus, i) => bus.map((_, i)) })
      .map({ case (bus, i) => (BigInt((-i) %+ bus), BigInt(bus)) })
      .toSeq
    NumberTheory.crt(ans)._1
    // TODO: answer always just Long?
  }

  def earliestSubsequentDepart(notes: Notes): BigInt = earliestSubsequentDepart(notes.buses)


  def parseBuses(s: String): Buses = {
    s.split(",").toSeq.map({
      case "x" => None
      case s => Some(s.toInt)
    })
  }

  def parseNotes(input: String): Notes = {
    val Seq(line1, line2) = input.linesIterator.take(2).toSeq
    Notes(line1.toInt, parseBuses(line2))
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(earliestBusWaitTime(parseNotes(input)))
    println(earliestSubsequentDepart(parseNotes(input)))

    // part 2: 1191202959664391 - too high (Long overflowed in crt)
  }
}
