package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day23._
import Day9.parseProgram

class Day23Test extends FunSuite {

  test("Part 1 input answer") {
    assert(runNetwork(parseProgram(input)) == 24954)
  }
}
