package eu.sim642.adventofcode2020

object Day13 {

  case class Notes(earliestDepart: Int, buses: Seq[Option[Int]])

  def earliestBusWaitTime(notes: Notes): Int = {
    val (earliestBus, waitTime) = notes.buses.view
      .flatten
      .map(bus => (bus, bus - notes.earliestDepart % bus)) // what about 0?
      .minBy(_._2)
    earliestBus * waitTime
  }


  def parseNotes(input: String): Notes = {
    val Seq(line1, line2) = input.linesIterator.take(2).toSeq
    val buses = line2.split(",").toSeq.map({
      case "x" => None
      case s => Some(s.toInt)
    })
    Notes(line1.toInt, buses)
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day13.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(earliestBusWaitTime(parseNotes(input)))
  }
}
