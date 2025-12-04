package eu.sim642.adventofcode2025

import Day4._
import org.scalatest.funsuite.AnyFunSuite

class Day4Test extends AnyFunSuite {

  val exampleInput =
    """..@@.@@@@.
      |@@@.@.@.@@
      |@@@@@.@.@@
      |@.@@@@..@.
      |@@.@@@@.@@
      |.@@@@@@@.@
      |.@.@.@.@@@
      |@.@@@.@@@@
      |.@@@@@@@@.
      |@.@.@@@.@.""".stripMargin

  test("Part 1 examples") {
    assert(countAccessibleRolls(parseGrid(exampleInput)) == 13)
  }

  test("Part 1 input answer") {
    assert(countAccessibleRolls(parseGrid(input)) == 1527)
  }
}
