package eu.sim642.adventofcode2016

import Day19._
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(Part1.lastElf(5) == 3)

    assert(Part1.lastElf(4) == 1)
    assert(Part1.lastElf(7) == 7)
  }

  test("Part 1 input answer") {
    assert(Part1.lastElf(input) == 1834903)
  }

  test("Part 2 examples") {
    assert(Part2.lastElf(5) == 2)
  }

  test("Part 2 input answer") {
    assert(Part2.lastElf(input) == 1420280)
  }
}
