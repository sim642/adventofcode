package eu.sim642.adventofcode2019

import Day19._
import intcode.parseProgram
import org.scalatest.funsuite.AnyFunSuite

class Day19Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(countTractorBeam(parseProgram(input)) == 110)
  }

  test("Part 2 input answer") {
    assert(findSquareValue(parseProgram(input)) == 17302065)
  }
}
