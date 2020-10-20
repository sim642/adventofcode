package eu.sim642.adventofcode2015

import Day20._
import org.scalatest.funsuite.AnyFunSuite

class Day20Test extends AnyFunSuite {

  test("Part 1 examples") {
    assert(findHouse(100) == 6)
    assert(findHouse(150) == 8)
  }

  test("Part 1 input answer") {
    assert(findHouse(input) == 776160)
  }

  test("Part 2 input answer") {
    assert(findHouseLimit(input) == 786240)
  }
}
