package eu.sim642.adventofcode2020

import Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  val exampleInput =
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#""".stripMargin

  test("Part 1 examples") {
    assert(countSlopeTrees(parseGrid(exampleInput)) == 7)
  }

  test("Part 1 input answer") {
    assert(countSlopeTrees(parseGrid(input)) == 164)
  }
}
