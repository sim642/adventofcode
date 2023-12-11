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
    assert(Part1.sumDistances(parseGalaxies(exampleInput)) == 374)
  }

  test("Part 1 input answer") {
    assert(Part1.sumDistances(parseGalaxies(input)) == 9509330)
  }

  test("Part 2 examples") {
    assert(Part2.sumDistances(parseGalaxies(exampleInput), 10) == 1030)
    assert(Part2.sumDistances(parseGalaxies(exampleInput), 100) == 8410)
  }

  test("Part 2 input answer") {
    assert(Part2.sumDistances(parseGalaxies(input)) == 635832237682L)
  }
}
