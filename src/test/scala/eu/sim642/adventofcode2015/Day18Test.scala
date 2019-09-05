package eu.sim642.adventofcode2015

import org.scalatest.FunSuite
import Day18._

class Day18Test extends FunSuite {

  val exampleInput =
    """.#.#.#
      |...##.
      |#....#
      |..#...
      |#.#..#
      |####..""".stripMargin

  test("Part 1 examples") {
    assert(countOnIterate(exampleInput, 4) == 4)
  }

  test("Part 1 input answer") {
    assert(countOnIterate(input) == 821)
  }
}
