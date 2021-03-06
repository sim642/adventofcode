package eu.sim642.adventofcode2020

import Day12._
import org.scalatest.funsuite.AnyFunSuite

class Day12Test extends AnyFunSuite {

  val exampleInput =
    """F10
      |N3
      |F7
      |R90
      |F11""".stripMargin

  test("Part 1 examples") {
    assert(Part1.movesDistance(parseMoves(exampleInput)) == 25)
  }

  test("Part 1 input answer") {
    assert(Part1.movesDistance(parseMoves(input)) == 858)
  }

  test("Part 2 examples") {
    assert(Part2.movesDistance(parseMoves(exampleInput)) == 286)
  }

  test("Part 2 input answer") {
    assert(Part2.movesDistance(parseMoves(input)) == 39140)
  }
}
