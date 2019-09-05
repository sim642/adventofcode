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
    assert(Part1.countOnIterate(exampleInput, 4) == 4)
  }

  test("Part 1 input answer") {
    assert(Part1.countOnIterate(input) == 821)
  }

  test("Part 2 examples") {
    assert(Part2.countOnIterate(exampleInput, 5) == 17)
  }

  test("Part 2 input answer") {
    assert(Part2.countOnIterate(input) == 886)
  }
}
