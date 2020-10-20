package eu.sim642.adventofcode2019

import Day15._
import intcode.parseProgram
import org.scalatest.funsuite.AnyFunSuite

class Day15Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(oxygenSystemMoves(parseProgram(input)) == 238)
  }

  test("Part 2 input answer") {
    assert(oxygenFillMinutes(parseProgram(input)) == 392)
  }
}
