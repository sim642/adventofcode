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

  test("Part 2 examples") {
    assert(sumDistances2(parseGalaxies(exampleInput), 10) == 1030)
    assert(sumDistances2(parseGalaxies(exampleInput), 100) == 8410)
  }

  test("Part 2 input answer") {
    assert(sumDistances2(parseGalaxies(input)) == 635832237682L)
  }
}
