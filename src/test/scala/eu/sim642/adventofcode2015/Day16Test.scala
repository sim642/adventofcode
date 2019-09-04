package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day16._

class Day16Test extends FunSuite {

  test("Part 1 input answer") {
    assert(Part1.findWrappingSue(input) == 373)
  }

  test("Part 2 input answer") {
    assert(Part2.findWrappingSue(input) == 260)
  }
}
