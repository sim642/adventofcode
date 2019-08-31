package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day9._

class Day9Test extends FunSuite {

  val exampleInput =
    """London to Dublin = 464
      |London to Belfast = 518
      |Dublin to Belfast = 141""".stripMargin

  test("Part 1 examples") {
    assert(shortestRoute(exampleInput) == 605)
  }

  test("Part 1 input answer") {
    assert(shortestRoute(input) == 207)
  }
}
