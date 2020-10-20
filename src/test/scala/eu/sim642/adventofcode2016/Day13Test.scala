package eu.sim642.adventofcode2016

import Day13._
import eu.sim642.adventofcodelib.pos.Pos
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

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

  test("Part 2 input answer") {
    assert(reachableLocations(input) == 127)
  }
}
