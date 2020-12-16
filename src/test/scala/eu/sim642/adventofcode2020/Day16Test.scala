package eu.sim642.adventofcode2020

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  val exampleInput =
    """class: 1-3 or 5-7
      |row: 6-11 or 33-44
      |seat: 13-40 or 45-50
      |
      |your ticket:
      |7,1,14
      |
      |nearby tickets:
      |7,3,47
      |40,4,50
      |55,2,20
      |38,6,12""".stripMargin

  val exampleInput2 =
    """class: 0-1 or 4-19
      |row: 0-5 or 8-19
      |seat: 0-13 or 16-19
      |
      |your ticket:
      |11,12,13
      |
      |nearby tickets:
      |3,9,18
      |15,1,5
      |5,14,9""".stripMargin

  test("parseInput") {
    val Input(fields, myTicket, nearbyTickets) = parseInput(exampleInput)
    assert(fields == Seq(
      Field("class", 1 to 3, 5 to 7),
      Field("row", 6 to 11, 33 to 44),
      Field("seat", 13 to 40, 45 to 50),
    ))
    assert(myTicket == Seq(7, 1, 14))
    assert(nearbyTickets == Seq(
      Seq(7, 3, 47),
      Seq(40, 4, 50),
      Seq(55, 2, 20),
      Seq(38, 6, 12),
    ))
  }

  test("Part 1 examples") {
    assert(ticketScanningErrorRate(parseInput(exampleInput)) == 71)
  }

  test("Part 1 input answer") {
    assert(ticketScanningErrorRate(parseInput(input)) == 26869)
  }

  test("Part 2 examples") {
    myTicketDepartureProduct(parseInput(exampleInput2))
  }
}
