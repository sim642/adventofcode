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
    assert(Part1.totalNorthLoad(parseGrid(exampleInput)) == 136)
  }

  test("Part 1 input answer") {
    assert(Part1.totalNorthLoad(parseGrid(input)) == 112048)
  }

  test("Part 2 examples") {
    assert(Part2.totalNorthLoad(parseGrid(exampleInput)) == 64)
  }

  test("Part 2 input answer") {
    assert(Part2.totalNorthLoad(parseGrid(input)) == 105606)
  }
}
