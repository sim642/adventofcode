package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day17._

class Day17Test extends FunSuite {

  val exampleGrid =
    """..#..........
      |..#..........
      |#######...###
      |#.#...#...#.#
      |#############
      |..#...#...#..
      |..#####...^..""".stripMargin

  test("Part 1 examples") {
    assert(sumAlignmentParameters(parseGrid(exampleGrid)) == 76)
  }

  test("Part 1 input answer") {
    assert(sumAlignmentParameters(parseInput(input)) == 3660)
  }
}
