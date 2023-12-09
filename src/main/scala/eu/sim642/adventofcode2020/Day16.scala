package eu.sim642.adventofcode2020

import eu.sim642.adventofcodelib.IteratorImplicits._

object Day16 {

  case class Field(name: String, range1: Range.Inclusive, range2: Range.Inclusive)

  type Ticket = Seq[Int]

  case class Input(fields: Seq[Field], myTicket: Ticket, nearbyTickets: Seq[Ticket])

  def fieldsInvalidValues(fields: Seq[Field]): Ticket => Seq[Int] = {
    val allValidValues =
      fields
        .view
        .flatMap({ case Field(_, range1, range2) =>
          range1.view ++ range2.view
        })
        .toSet

    _.filterNot(allValidValues)
  }

  def ticketScanningErrorRate(input: Input): Int = {
    val Input(fields, _, nearbyTickets) = input
    val invalidValues = fieldsInvalidValues(fields)

    nearbyTickets
      .flatMap(invalidValues)
      .sum
  }

  def myTicketDepartureProduct(input: Input): Long = {
    val Input(fields, myTicket, nearbyTickets) = input
    val invalidValues = fieldsInvalidValues(fields)
    val validNearbyTickets = nearbyTickets.filter(invalidValues(_).isEmpty)

    val fieldValidValues = fields.map({ case field@Field(_, range1, range2) =>
      field -> (range1.view ++ range2.view).toSet
    })
    val columnValues =
      (myTicket +: validNearbyTickets)
        .transpose
        .map(_.toSet)
    val columnValidFields = columnValues.map(columnValue =>
      fieldValidValues
        .view
        .collect({ case (field, validValue) if columnValue.subsetOf(validValue) => field })
        .toSet
    )

    def helper[A](columnValidFields: List[(Set[Field], A)], usedFields: Set[Field]): Iterator[List[(Field, A)]] = {
      columnValidFields match {
        case Nil =>
          Iterator(Nil)
        case (columnValidField, aux) :: newColumnValidFields =>
          for {
            field <- columnValidField.iterator
            if !usedFields.contains(field)
            rest <- helper(newColumnValidFields, usedFields + field)
          } yield (field, aux) :: rest
      }
    }

    val columnValidFieldsSorted =
      columnValidFields
        .zipWithIndex
        .sortBy(_._1.size)
        .toList
    val fieldOrder =
      helper(columnValidFieldsSorted, Set.empty)
        .head
        .sortBy(_._2)
        .map(_._1)

    fieldOrder
      .lazyZip(myTicket)
      .collect({ case (Field(name, _, _), value) if name.startsWith("departure") => value.toLong })
      .product
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

  lazy val input: String = scala.io.Source.fromInputStream(getClass.getResourceAsStream("day16.txt")).mkString.trim

  def main(args: Array[String]): Unit = {
    println(ticketScanningErrorRate(parseInput(input)))
    println(myTicketDepartureProduct(parseInput(input)))

    // part 2: 577037097 - too low (Int overflowed in product)
  }
}
