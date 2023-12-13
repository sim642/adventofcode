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
    assert(Part1.summarizeMirrors(parseGrids(exampleInput)) == 405)
  }

  test("Part 1 input answer") {
    assert(Part1.summarizeMirrors(parseGrids(input)) == 30802)
  }

  test("Part 2 examples") {
    assert(Part2.summarizeMirrors(parseGrids(exampleInput)) == 400)
  }

  test("Part 2 input answer") {
    assert(Part2.summarizeMirrors(parseGrids(input)) == 37876)
  }
}
