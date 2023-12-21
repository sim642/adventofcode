package eu.sim642.adventofcode2023

import Day21._
import org.scalatest.funsuite.AnyFunSuite

class Day21Test extends AnyFunSuite {

  private val exampleInput =
    """...........
      |.....###.#.
      |.###.##..#.
      |..#.#...#..
      |....#.#....
      |.##..S####.
      |.##..#...#.
      |.......##..
      |.##.#.####.
      |.##..##.##.
      |...........""".stripMargin

  test("Part 1 examples") {
    assert(countReachableExactly(parseGrid(exampleInput), 6) == 16)
  }

  test("Part 1 input answer") {
    assert(countReachableExactly(parseGrid(input)) == 3687)
  }
}
