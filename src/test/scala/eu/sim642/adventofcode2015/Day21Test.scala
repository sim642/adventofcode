package eu.sim642.adventofcode2015

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(leastWinGold(input) == 111)
  }

  test("Part 2 input answer") {
    assert(mostLoseGold(input) == 188)
  }
}
