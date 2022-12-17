package eu.sim642.adventofcode2022

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  val exampleInput =
    """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"""

  test("Part 1 examples") {
    assert(towerHeight(parseJets(exampleInput)) == 3068)
  }

  test("Part 1 input answer") {
    assert(towerHeight(parseJets(input)) == 3111)
  }
}
