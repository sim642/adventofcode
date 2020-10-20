package eu.sim642.adventofcode2015

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(Part1.findWrappingSue(input) == 373)
  }

  test("Part 2 input answer") {
    assert(Part2.findWrappingSue(input) == 260)
  }
}
