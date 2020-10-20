package eu.sim642.adventofcode2019

import Day25._
import intcode.parseProgram
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(findPassword(parseProgram(input)) == 2622472)
  }
}
