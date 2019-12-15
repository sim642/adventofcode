package eu.sim642.adventofcode2019

import Day15._
import Day9.parseProgram
import org.scalatest.FunSuite

class Day15Test extends FunSuite {

  test("Part 1 input answer") {
    assert(oxygenSystemMoves(parseProgram(input)) == 238)
  }
}
