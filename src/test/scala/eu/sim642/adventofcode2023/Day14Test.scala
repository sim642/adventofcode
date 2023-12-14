package eu.sim642.adventofcode2023

import Day14._
import org.scalatest.funsuite.AnyFunSuite

class Day14Test extends AnyFunSuite {

  private val exampleInput =
    """O....#....
      |O.OO#....#
      |.....##...
      |OO.#O....O
      |.O.....O#.
      |O.#..O.#.#
      |..O..#O..O
      |.......O..
      |#....###..
      |#OO..#....""".stripMargin

  test("Part 1 examples") {
    assert(totalLoad(parseGrid(exampleInput)) == 136)
  }

  test("Part 1 input answer") {
    assert(totalLoad(parseGrid(input)) == 112048)
  }

  test("Part 2 examples") {
    assert(totalLoad2(parseGrid(exampleInput)) == 64)
  }

  test("Part 2 input answer") {
    assert(totalLoad2(parseGrid(input)) == 105606)
  }
}
