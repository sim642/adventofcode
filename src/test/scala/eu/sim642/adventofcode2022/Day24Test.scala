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
    assert(fewestMinutes(parseInput(exampleInput)) == 18)
  }

  test("Part 1 input answer") {
    assert(fewestMinutes(parseInput(input)) == 253) // TODO: optimize, 3.5s
  }

  test("Part 2 examples") {
    assert(fewestMinutes2(parseInput(exampleInput)) == 54)
  }

  ignore("Part 2 input answer") {
    assert(fewestMinutes2(parseInput(input)) == 794) // TODO: optimize, 31.2s
  }
}
