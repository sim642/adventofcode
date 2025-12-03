package eu.sim642.adventofcode2025

import Day3._
import org.scalatest.funsuite.AnyFunSuite

class Day3Test extends AnyFunSuite {

  val exampleInput =
    """987654321111111
      |811111111111119
      |234234234234278
      |818181911112111""".stripMargin

  test("Part 1 examples") {
    assert(totalJoltage(parseBanks(exampleInput)) == 357)
  }

  test("Part 1 input answer") {
    assert(totalJoltage(parseBanks(input)) == 17301)
  }
}
