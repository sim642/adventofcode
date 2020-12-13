package eu.sim642.adventofcode2020

import Day13._
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  val exampleInput =
    """939
      |7,13,x,x,59,x,31,19""".stripMargin

  test("Part 1 examples") {
    assert(earliestBusWaitTime(parseNotes(exampleInput)) == 295)
  }

  test("Part 1 input answer") {
    assert(earliestBusWaitTime(parseNotes(input)) == 203)
  }
}
