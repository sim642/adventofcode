package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day21._
import Day9.parseProgram

class Day21Test extends FunSuite {

  test("Part 1 input answer") {
    assert(hullDamageWalk(parseProgram(input)) == 19358262)
  }

  test("Part 2 input answer") {
    assert(hullDamageRun(parseProgram(input)) == 1142686742)
  }
}
