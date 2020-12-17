package eu.sim642.adventofcode2020

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  val exampleInput =
    """.#.
      |..#
      |###""".stripMargin

  test("Part 1 examples") {
    assert(Part1.countCubesBooted(parseGrid(exampleInput)) == 112)
  }

  test("Part 1 input answer") {
    assert(Part1.countCubesBooted(parseGrid(input)) == 276)
  }

  test("Part 2 examples") {
    assert(Part2.countCubesBooted(parseGrid(exampleInput)) == 848)
  }

  test("Part 2 input answer") {
    assert(Part2.countCubesBooted(parseGrid(input)) == 2136)
  }
}
