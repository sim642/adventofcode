package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day20._

class Day20Test extends FunSuite {

  val exampleInput =
    """5-8
      |0-2
      |4-7""".stripMargin

  test("Part 1 examples") {
    assert(minUnblocked(exampleInput) == 3)
  }

  test("Part 1 input answer") {
    assert(minUnblocked(input) == 4793564)
  }

  test("Part 2 examples") {
    assert(countUnblocked(exampleInput, Interval(0, 9)) == 2)
  }

  test("Part 2 input answer") {
    assert(countUnblocked(input) == 146)
  }
}
