package eu.sim642.adventofcode2023

import Day13._
import org.scalatest.funsuite.AnyFunSuite

class Day13Test extends AnyFunSuite {

  private val exampleInput =
    """#.##..##.
      |..#.##.#.
      |##......#
      |##......#
      |..#.##.#.
      |..##..##.
      |#.#.##.#.
      |
      |#...##..#
      |#....#..#
      |..##..###
      |#####.##.
      |#####.##.
      |..##..###
      |#....#..#""".stripMargin

  test("Part 1 examples") {
    assert(summarizeMirrors(parseGrids(exampleInput)) == 405)
  }

  test("Part 1 input answer") {
    assert(summarizeMirrors(parseGrids(input)) == 30802)
  }
}
