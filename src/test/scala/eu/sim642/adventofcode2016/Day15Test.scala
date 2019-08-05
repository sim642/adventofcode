package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day15._

class Day15Test extends FunSuite {

  val exampleInput =
    """Disc #1 has 5 positions; at time=0, it is at position 4.
      |Disc #2 has 2 positions; at time=0, it is at position 1.""".stripMargin

  test("Part 1 examples") {
    assert(Part1.firstPressTime(exampleInput) == 5)
  }

  test("Part 1 input answer") {
    assert(Part1.firstPressTime(input) == 121834)
  }

  test("Part 2 input answer") {
    assert(Part2.firstPressTime(input) == 3208099)
  }
}
