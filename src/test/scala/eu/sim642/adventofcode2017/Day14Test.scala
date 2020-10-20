package eu.sim642.adventofcode2017

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  test("Part 1 example") {
    assert(squaresUsed("flqrgnkx") == 8108)
  }

  test("Part 1 input answer") {
    assert(squaresUsed(input) == 8140)
  }

  test("Part 2 example") {
    assert(regionsCount("flqrgnkx") == 1242)
  }

  test("Part 2 input answer") {
    assert(regionsCount(input) == 1182)
  }
}
