package eu.sim642.adventofcode2023

import Day18._
import org.scalatest.funsuite.AnyFunSuite

class Day18Test extends AnyFunSuite {

  private val exampleInput =
    """R 6 (#70c710)
      |D 5 (#0dc571)
      |L 2 (#5713f0)
      |D 2 (#d2c081)
      |R 2 (#59c680)
      |D 2 (#411b91)
      |L 5 (#8ceee2)
      |U 2 (#caa173)
      |L 1 (#1b58a2)
      |U 2 (#caa171)
      |R 2 (#7807d2)
      |U 3 (#a77fa3)
      |L 2 (#015232)
      |U 2 (#7a21e3)""".stripMargin

  test("Part 1 examples") {
    assert(Part1.lagoonSize(parseDigPlan(exampleInput)) == 62)
  }

  test("Part 1 input answer") {
    assert(Part1.lagoonSize(parseDigPlan(input)) == 70026)
  }

  test("Part 2 examples") {
    assert(Part2.parseColor("70c710") == ('R', 461937))
    assert(Part2.lagoonSize(parseDigPlan(exampleInput)) == 952408144115L)
  }

  test("Part 2 input answer") {
    assert(Part2.lagoonSize(parseDigPlan(input)) == 68548301037382L)
  }
}
