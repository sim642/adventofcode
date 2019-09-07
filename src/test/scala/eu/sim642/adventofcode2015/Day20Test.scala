package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day20._

class Day20Test extends FunSuite {

  test("Part 1 examples") {
    assert(findHouse(100) == 6)
    assert(findHouse(150) == 8)
  }

  test("Part 1 input answer") {
    assert(findHouse(input) == 776160)
  }
}
