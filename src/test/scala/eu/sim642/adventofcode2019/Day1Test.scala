package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day1._

class Day1Test extends FunSuite {

  test("Part 1 examples") {
    assert(Part1.requiredFuel(12) == 2)
    assert(Part1.requiredFuel(14) == 2)
    assert(Part1.requiredFuel(1969) == 654)
    assert(Part1.requiredFuel(100756) == 33583)
  }

  test("Part 1 input answer") {
    assert(Part1.totalRequiredFuel(parseMasses(input)) == 3371958)
  }

  test("Part 2 examples") {
    assert(Part2.requiredFuel(12) == 2)
    assert(Part2.requiredFuel(1969) == 966)
    assert(Part2.requiredFuel(100756) == 50346)
  }

  test("Part 2 input answer") {
    assert(Part2.totalRequiredFuel(parseMasses(input)) == 5055050)
  }
}
