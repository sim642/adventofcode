package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day23._
import Intcode.parseProgram

class Day23Test extends FunSuite {

  test("Part 1 input answer") {
    assert(runNetwork(parseProgram(input)) == 24954)
  }

  test("Part 2 input answer") {
    assert(runNetworkNat(parseProgram(input)) == 17091)
  }
}
