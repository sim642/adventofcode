package eu.sim642.adventofcode2024

import Day25._
import org.scalatest.funsuite.AnyFunSuite

class Day25Test extends AnyFunSuite {

  val exampleInput =
    """#####
      |.####
      |.####
      |.####
      |.#.#.
      |.#...
      |.....
      |
      |#####
      |##.##
      |.#.##
      |...##
      |...#.
      |...#.
      |.....
      |
      |.....
      |#....
      |#....
      |#...#
      |#.#.#
      |#.###
      |#####
      |
      |.....
      |.....
      |#.#..
      |###..
      |###.#
      |###.#
      |#####
      |
      |.....
      |.....
      |.....
      |#....
      |#.#..
      |#.#.#
      |#####""".stripMargin

  test("Part 1 examples") {
    assert(countLockKeyFits(parseLockKeys(exampleInput)) == 3)
  }

  test("Part 1 input answer") {
    assert(countLockKeyFits(parseLockKeys(input)) == 3264)
  }
}
