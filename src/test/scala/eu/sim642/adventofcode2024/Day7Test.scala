package eu.sim642.adventofcode2024

import Day7._
import org.scalatest.funsuite.AnyFunSuite

class Day7Test extends AnyFunSuite {

  val exampleInput =
    """190: 10 19
      |3267: 81 40 27
      |83: 17 5
      |156: 15 6
      |7290: 6 8 6 15
      |161011: 16 10 13
      |192: 17 8 14
      |21037: 9 7 18 13
      |292: 11 6 16 20""".stripMargin

  test("Part 1 examples") {
    assert(totalCalibrationResult(parseInput(exampleInput)) == 3749)
  }

  test("Part 1 input answer") {
    assert(totalCalibrationResult(parseInput(input)) == 1620690235709L)
  }
}
