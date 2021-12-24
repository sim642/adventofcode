package eu.sim642.adventofcode2021

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  test("Part 1 input answer") {
    assert(maxModelNumber(parseSteps(input)) == "97919997299495")
  }

  test("Part 2 input answer") {
    assert(minModelNumber(parseSteps(input)) == "51619131181131")
  }
}
