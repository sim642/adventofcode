package eu.sim642.adventofcode2023

import Day6._
import org.scalatest.funsuite.AnyFunSuite

class Day6Test extends AnyFunSuite {

  private val exampleInput =
    """Time:      7  15   30
      |Distance:  9  40  200""".stripMargin

  test("Part 1 examples") {
    assert(multiplyRaceWins(parseRaces(exampleInput)) == 288)
  }

  test("Part 1 input answer") {
    assert(multiplyRaceWins(parseRaces(input)) == 2756160)
  }
}
