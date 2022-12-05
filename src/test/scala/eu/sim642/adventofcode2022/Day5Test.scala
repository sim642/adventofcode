package eu.sim642.adventofcode2022

import Day5._
import org.scalatest.funsuite.AnyFunSuite

class Day5Test extends AnyFunSuite {

  val exampleInput =
    """    [D]
      |[N] [C]
      |[Z] [M] [P]
      | 1   2   3
      |
      |move 1 from 2 to 1
      |move 3 from 1 to 3
      |move 2 from 2 to 1
      |move 1 from 1 to 2
      |""".stripMargin

  test("Part 1 examples") {
    assert(Part1.topMessage(parseInput(exampleInput)) == "CMZ")
  }

  test("Part 1 input answer") {
    assert(Part1.topMessage(parseInput(input)) == "TLNGFGMFN")
  }

  test("Part 2 examples") {
    assert(Part2.topMessage(parseInput(exampleInput)) == "MCD")
  }

  test("Part 2 input answer") {
    assert(Part2.topMessage(parseInput(input)) == "FGLQJCMBD")
  }
}
