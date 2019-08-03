package eu.sim642.adventofcode2016

import org.scalatest.FunSuite
import Day13._
import eu.sim642.adventofcode2017.Day3.Pos

class Day13Test extends FunSuite {

  val exampleInput = 10

  test("Part 1 examples") {
    assert(isOpen(Pos(0, 0), exampleInput))
    assert(!isOpen(Pos(1, 0), exampleInput))
    assert(isOpen(Pos(0, 1), exampleInput))
    assert(isOpen(Pos(1, 1), exampleInput))

    assert(fewestSteps(exampleInput, Pos(7, 4)) == 11)
  }

  test("Part 1 input answer") {
    assert(fewestSteps(input) == 86)
  }
}
