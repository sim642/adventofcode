package eu.sim642.adventofcode2015

import Day9._
import org.scalatest.funsuite.AnyFunSuite

class Day9Test extends AnyFunSuite {

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

  test("Part 2 examples") {
    assert(longestRoute(exampleInput) == 982)
  }

  test("Part 2 input answer") {
    assert(longestRoute(input) == 804)
  }
}
