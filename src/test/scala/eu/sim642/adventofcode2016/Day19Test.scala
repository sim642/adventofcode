package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day19._

class Day19Test extends FunSuite {

  test("Part 1 examples") {
    assert(lastElf(5) == 3)

    assert(lastElf(4) == 1)
    assert(lastElf(7) == 7)
  }

  test("Part 1 input answer") {
    assert(lastElf(input) == 1834903)
  }
}
