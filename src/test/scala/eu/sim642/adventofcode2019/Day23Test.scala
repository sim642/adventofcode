package eu.sim642.adventofcode2019

import Day23._
import intcode.parseProgram
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(runNetwork(parseProgram(input)) == 24954)
  }

  test("Part 2 input answer") {
    assert(runNetworkNat(parseProgram(input)) == 17091)
  }
}
