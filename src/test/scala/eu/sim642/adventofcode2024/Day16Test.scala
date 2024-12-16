package eu.sim642.adventofcode2024

import Day16._
import org.scalatest.funsuite.AnyFunSuite

class Day16Test extends AnyFunSuite {

  val exampleInput =
    """###############
      |#.......#....E#
      |#.#.###.#.###.#
      |#.....#.#...#.#
      |#.###.#####.#.#
      |#.#.#.......#.#
      |#.#.#####.###.#
      |#...........#.#
      |###.#.#####.#.#
      |#...#.....#.#.#
      |#.#.#.###.#.#.#
      |#.....#...#.#.#
      |#.###.#.#.#.#.#
      |#S..#.....#...#
      |###############""".stripMargin

  val exampleInput2 =
    """#################
      |#...#...#...#..E#
      |#.#.#.#.#.#.#.#.#
      |#.#.#.#...#...#.#
      |#.#.#.#.###.#.#.#
      |#...#.#.#.....#.#
      |#.#.#.#.#.#####.#
      |#.#...#.#.#.....#
      |#.#.#####.#.###.#
      |#.#.#.......#...#
      |#.#.###.#####.###
      |#.#.#...#.....#.#
      |#.#.#.#####.###.#
      |#.#.#.........#.#
      |#.#.#.#########.#
      |#S#.............#
      |#################""".stripMargin

  test("Part 1 examples") {
    assert(lowestScore(parseGrid(exampleInput)) == 7036)
    assert(lowestScore(parseGrid(exampleInput2)) == 11048)
  }

  test("Part 1 input answer") {
    assert(lowestScore(parseGrid(input)) == 73404)
  }

  test("Part 2 examples") {
    assert(bestPathTiles(parseGrid(exampleInput)) == 45)
    assert(bestPathTiles(parseGrid(exampleInput2)) == 64)
  }

  test("Part 2 input answer") {
    assert(bestPathTiles(parseGrid(input)) == 449)
  }
}
