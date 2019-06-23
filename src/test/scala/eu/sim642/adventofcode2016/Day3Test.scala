package eu.sim642.adventofcode2016

import Day3._
import org.scalatest.FunSuite

class Day3Test extends FunSuite {

  test("Part 1 examples") {
    assert(!isValidTriangle(5, 10, 25))
    assert(!isValidTriangle(5, 25, 10))
    assert(!isValidTriangle(10, 5, 25))
    assert(!isValidTriangle(10, 25, 5))
    assert(!isValidTriangle(25, 5, 10))
    assert(!isValidTriangle(25, 10, 5))

    assert(isValidTriangle(5, 10, 10))
    assert(isValidTriangle(10, 5, 10))
    assert(isValidTriangle(10, 10, 5))
  }

  test("Part 1 input answer") {
    assert(Part1.countPossibleTriangles(input) == 1050)
  }

  test("Part 2 input answer") {
    assert(Part2.countPossibleTriangles(input) == 1921)
  }
}
