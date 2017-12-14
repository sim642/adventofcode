package eu.sim642.adventofcode2017

import Day14._
import org.scalatest.FunSuite

class Day14Test extends FunSuite {

  test("Part 1 example") {
    assert(squaresUsed("flqrgnkx") == 8108)
  }

  test("Part 1 input answer") {
    assert(squaresUsed(input) == 8140)
  }
}
