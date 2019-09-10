package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day23._

class Day23Test extends FunSuite {

  test("Part 1 input answer") {
    assert(Part1.execRegisterB(input) == 184)
  }

  test("Part 2 input answer") {
    assert(Part2.execRegisterB(input) == 231)
  }
}
