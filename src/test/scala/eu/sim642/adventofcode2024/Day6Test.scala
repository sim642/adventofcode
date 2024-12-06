package eu.sim642.adventofcode2024

import Day6._
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  val exampleInput =
    """....#.....
      |.........#
      |..........
      |..#.......
      |.......#..
      |..........
      |.#..^.....
      |........#.
      |#.........
      |......#...""".stripMargin

  test("Part 1 examples") {
    assert(countGuardPoss(parseInput(exampleInput)) == 41)
  }

  test("Part 1 input answer") {
    assert(countGuardPoss(parseInput(input)) == 4752)
  }

  test("Part 2 examples") {
    assert(countObstructionPoss(parseInput(exampleInput)) == 6)
  }

  test("Part 2 input answer") {
    assert(countObstructionPoss(parseInput(input)) == 1719)
  }
}
