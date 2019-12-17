package eu.sim642.adventofcode2019

import org.scalatest.FunSuite
import Day17._
import Day9.parseProgram

class Day17Test extends FunSuite {

  val exampleGrid =
    """..#..........
      |..#..........
      |#######...###
      |#.#...#...#.#
      |#############
      |..#...#...#..
      |..#####...^..""".stripMargin

  val exampleGrid2 =
    """#######...#####
      |#.....#...#...#
      |#.....#...#...#
      |......#...#...#
      |......#...###.#
      |......#.....#.#
      |^########...#.#
      |......#.#...#.#
      |......#########
      |........#...#..
      |....#########..
      |....#...#......
      |....#...#......
      |....#...#......
      |....#####......""".stripMargin

  test("Part 1 examples") {
    assert(sumAlignmentParameters(parseGrid(exampleGrid)) == 76)
  }

  test("Part 1 input answer") {
    assert(sumAlignmentParameters(parseProgram(input)) == 3660)
  }

  ignore("Part 2 examples") {
    //assert(dustCollected(parseGrid(exampleGrid2)) == 0)
  }

  test("Part 2 input answer") {
    assert(dustCollected(parseProgram(input)) == 962913)
  }
}
