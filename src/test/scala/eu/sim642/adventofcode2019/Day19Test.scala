package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day19._
import Intcode.parseProgram

class Day19Test extends FunSuite {

  test("Part 1 input answer") {
    assert(countTractorBeam(parseProgram(input)) == 110)
  }

  test("Part 2 input answer") {
    assert(findSquareValue(parseProgram(input)) == 17302065)
  }
}
