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

  test("Part 2 examples") {
    assert(countReachableExactlyInfinite(parseGrid(exampleInput), 6) == 16)
    assert(countReachableExactlyInfinite(parseGrid(exampleInput), 10) == 50)
    assert(countReachableExactlyInfinite(parseGrid(exampleInput), 50) == 1594)
    assert(countReachableExactlyInfinite(parseGrid(exampleInput), 100) == 6536)
    assert(countReachableExactlyInfinite(parseGrid(exampleInput), 500) == 167004)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 1000) == 668697)
    //assert(countReachableExactlyInfinite(parseGrid(exampleInput), 5000) == 16733044)
  }

  test("Part 2 input answer") {
    //assert(countReachableExactlyInfinite(parseGrid(input)) == 3687)
  }
}
