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
    assert(longestHike(parseGrid(exampleInput)) == 94)
  }

  ignore("Part 1 input answer") { // TODO: optimize
    assert(longestHike(parseGrid(input)) == 2414)
  }
}
