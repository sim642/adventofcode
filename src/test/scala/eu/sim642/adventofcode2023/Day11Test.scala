package eu.sim642.adventofcode2023

import Day11._
import org.scalatest.funsuite.AnyFunSuite

class Day11Test extends AnyFunSuite {

  private val exampleInput =
    """...#......
      |.......#..
      |#.........
      |..........
      |......#...
      |.#........
      |.........#
      |..........
      |.......#..
      |#...#.....""".stripMargin

  test("Part 1 examples") {
    assert(sumDistances(parseGalaxies(exampleInput)) == 374)
  }

  test("Part 1 input answer") {
    assert(sumDistances(parseGalaxies(input)) == 9509330)
  }
}
