package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day21._
import Intcode.parseProgram

class Day21Test extends FunSuite {

  test("Part 1 input answer") {
    assert(Part1.hullDamage(parseProgram(input)) == 19358262)
  }

  test("Part 2 input answer") {
    assert(Part2.hullDamage(parseProgram(input)) == 1142686742)
  }
}
