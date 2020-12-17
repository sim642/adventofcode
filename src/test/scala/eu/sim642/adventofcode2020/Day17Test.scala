package eu.sim642.adventofcode2020

import Day17._
import org.scalatest.funsuite.AnyFunSuite

class Day17Test extends AnyFunSuite {

  val exampleInput =
    """.#.
      |..#
      |###""".stripMargin

  test("Part 1 examples") {
    assert(countCubesBooted(parseInitialState(exampleInput)) == 112)
  }

  test("Part 1 input answer") {
    assert(countCubesBooted(parseInitialState(input)) == 276)
  }
}
