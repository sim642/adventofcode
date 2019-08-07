package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day22._

class Day22Test extends FunSuite {

  test("Part 1 input answer") {
    assert(countViablePairs(input) == 941)
  }
}
