package eu.sim642.adventofcode2023

import Day23._
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  private val exampleInput =
    """#.#####################
      |#.......#########...###
      |#######.#########.#.###
      |###.....#.>.>.###.#.###
      |###v#####.#v#.###.#.###
      |###.>...#.#.#.....#...#
      |###v###.#.#.#########.#
      |###...#.#.#.......#...#
      |#####.#.#.#######.#.###
      |#.....#.#.#.......#...#
      |#.#####.#.#.#########v#
      |#.#...#...#...###...>.#
      |#.#.#v#######v###.###v#
      |#...#.>.#...>.>.#.###.#
      |#####v#.#.###v#.#.###.#
      |#.....#...#...#.#.#...#
      |#.#########.###.#.#.###
      |#...###...#...#...#.###
      |###.###.#.###v#####v###
      |#...#...#.#.>.>.#.>.###
      |#.###.###.#.###.#.#v###
      |#.....###...###...#...#
      |#####################.#""".stripMargin

  test("Part 1 examples") {
    assert(Part1.longestHike(parseGrid(exampleInput)) == 94)
  }

  test("Part 1 input answer") {
    assert(Part1.longestHike(parseGrid(input)) == 2414)
  }

  test("Part 2 examples") {
    assert(Part2.longestHike(parseGrid(exampleInput)) == 154)
  }

  ignore("Part 2 input answer") { // TODO: optimize
    assert(Part2.longestHike(parseGrid(input)) == 6598)
  }
}
