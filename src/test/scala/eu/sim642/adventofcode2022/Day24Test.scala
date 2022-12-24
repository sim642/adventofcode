package eu.sim642.adventofcode2022

import Day24._
import org.scalatest.funsuite.AnyFunSuite

class Day24Test extends AnyFunSuite {

  val exampleInput =
    """#.######
      |#>>.<^<#
      |#.<..<<#
      |#>v.><>#
      |#<^v^^>#
      |######.#""".stripMargin

  test("Part 1 examples") {
    assert(Part1.fewestMinutes(parseInput(exampleInput)) == 18)
  }

  test("Part 1 input answer") {
    assert(Part1.fewestMinutes(parseInput(input)) == 253) // TODO: optimize, 3.5s
  }

  test("Part 2 examples") {
    assert(Part2.fewestMinutes(parseInput(exampleInput)) == 54)
  }

  ignore("Part 2 input answer") {
    assert(Part2.fewestMinutes(parseInput(input)) == 794) // TODO: optimize, 31.2s
  }
}
