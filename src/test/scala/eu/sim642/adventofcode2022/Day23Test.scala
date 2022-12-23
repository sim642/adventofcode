package eu.sim642.adventofcode2022

import Day23._
import org.scalatest.funsuite.AnyFunSuite

class Day23Test extends AnyFunSuite {

  val exampleInput1 =
    """....#..
      |..###.#
      |#...#.#
      |.#...##
      |#.###..
      |##.#.##
      |.#..#..""".stripMargin

  val exampleInput2 =
    """.....
      |..##.
      |..#..
      |.....
      |..##.
      |.....""".stripMargin

  test("Part 1 examples") {
    assert(simulateEmpty(parseElves(exampleInput1)) == 110)
    assert(simulateEmpty(parseElves(exampleInput2)) == 5 * 6 - 5)
  }

  test("Part 1 input answer") {
    assert(simulateEmpty(parseElves(input)) == 3920)
  }

  test("Part 2 examples") {
    assert(countRounds(parseElves(exampleInput1)) == 20)
  }

  test("Part 2 input answer") {
    assert(countRounds(parseElves(input)) == 889) // TODO: optimize, ~3s
  }
}
