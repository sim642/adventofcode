package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day16 {

  case class Field(name: String, range1: Range.Inclusive, range2: Range.Inclusive)

  type Ticket = Seq[Int]

  case class Input(fields: Seq[Field], myTicket: Ticket, nearbyTickets: Seq[Ticket])

  def ticketScanningErrorRate(input: Input): Int = {
    val Input(fields, _, nearbyTickets) = input

    val allValidValues =
      fields
        .view
        .flatMap({ case Field(_, range1, range2) =>
          range1.view ++ range2.view
        })
        .toSet

    def invalidValues(ticket: Ticket): Seq[Int] = ticket.filterNot(allValidValues)

    nearbyTickets
      .flatMap(invalidValues)
      .sum
  }


  private val fieldRegex = """([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)""".r

  def parseField(s: String): Field = s match {
    case fieldRegex(name, range1Min, range1Max, range2Min, range2Max) =>
      Field(name, range1Min.toInt to range1Max.toInt, range2Min.toInt to range2Max.toInt)
  }

  def parseTicket(s: String): Ticket = s.split(",").toSeq.map(_.toInt)

  def parseInput(input: String): Input = {
    val Seq(fields, myTicket, nearbyTickets) = input.split("\n\n").toSeq
    Input(
      fields = fields.linesIterator.map(parseField).toSeq,
      myTicket = parseTicket(myTicket.linesIterator(1)),
      nearbyTickets = nearbyTickets.linesIterator.drop(1).map(parseTicket).toSeq
    )
  }

  lazy val input: String = io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(ticketScanningErrorRate(parseInput(input)))
  }
}
