package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day1._

class Day1Test extends FunSuite {

  test("Part 1 examples") {
    assert(requiredFuel(12) == 2)
    assert(requiredFuel(14) == 2)
    assert(requiredFuel(1969) == 654)
    assert(requiredFuel(100756) == 33583)
  }

  test("Part 1 input answer") {
    assert(totalRequiredFuel(parseMasses(input)) == 3371958)
  }
}
