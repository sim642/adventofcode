package eu.sim642.adventofcode2022

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """A Y
      |B X
      |C Z""".stripMargin

  test("Part 1 examples") {
    assert(Part1.strategyScore(parseStrategy(exampleInput)) == 15)
  }

  test("Part 1 input answer") {
    assert(Part1.strategyScore(parseStrategy(input)) == 12458)
  }

  test("Part 2 examples") {
    assert(Part2.strategyScore(parseStrategy(exampleInput)) == 12)
  }

  test("Part 2 input answer") {
    assert(Part2.strategyScore(parseStrategy(input)) == 12683)
  }
}
