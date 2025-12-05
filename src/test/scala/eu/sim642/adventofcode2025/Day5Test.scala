package eu.sim642.adventofcode2025

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  val exampleInput =
    """3-5
      |10-14
      |16-20
      |12-18
      |
      |1
      |5
      |8
      |11
      |17
      |32""".stripMargin

  test("Part 1 examples") {
    assert(countFreshAvailable(parseDatabase(exampleInput)) == 3)
  }

  test("Part 1 input answer") {
    assert(countFreshAvailable(parseDatabase(input)) == 679)
  }
}
