package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day21._

class Day21Test extends FunSuite {

  test("Part 1 input answer") {
    assert(leastWinGold(input) == 111)
  }

  test("Part 2 input answer") {
    assert(mostLoseGold(input) == 188)
  }
}
