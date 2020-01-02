package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day25._
import Intcode.parseProgram

class Day25Test extends FunSuite {

  test("Part 1 input answer") {
    assert(findPassword(parseProgram(input)) == 2622472)
  }
}
