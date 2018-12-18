package eu.sim642.adventofcode2018

import org.scalatest.FunSuite
import Day18._

class Day18Test extends FunSuite {

  val exampleInput =
    """.#.#...|#.
      |.....#|##|
      |.|..|...#.
      |..|#.....#
      |#.#|||#|#|
      |...#.||...
      |.|....|...
      |||...#|.#|
      ||.||||..|.
      |...#.|..|.""".stripMargin

  test("Part 1 examples") {
    assert(resourceValue(parseInput(exampleInput)) == 1147)
  }

  test("Part 1 input answer") {
    assert(resourceValue(parseInput(input)) == 653184)
  }
}
