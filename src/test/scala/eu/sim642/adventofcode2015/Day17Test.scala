package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day17._

class Day17Test extends FunSuite {

  test("Part 1 examples") {
    assert(countCombinations(List(20, 15, 10, 5, 5), 25) == 4)
  }

  test("Part 1 input answer") {
    assert(countCombinations(input) == 654)
  }

  test("Part 2 examples") {
    assert(countMinCombinations(List(20, 15, 10, 5, 5), 25) == 3)
  }

  test("Part 2 input answer") {
    assert(countMinCombinations(input) == 57)
  }
}
