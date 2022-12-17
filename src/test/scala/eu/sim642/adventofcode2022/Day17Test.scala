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

  test("Part 2 examples") {
    assert(towerHeight2(parseJets(exampleInput)) == 1514285714288L)
  }

  test("Part 2 input answer") {
    assert(towerHeight2(parseJets(input)) == 1526744186042L)
  }
}
