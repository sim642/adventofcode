package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day19._
import Day9.parseProgram

class Day19Test extends FunSuite {

  test("Part 1 input answer") {
    assert(countTractorBeam(parseProgram(input)) == 110)
  }
}
