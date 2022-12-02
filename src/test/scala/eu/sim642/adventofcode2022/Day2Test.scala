package eu.sim642.adventofcode2022

import Day2._
import org.scalatest.funsuite.AnyFunSuite

class Day2Test extends AnyFunSuite {

  val exampleInput =
    """A Y
      |B X
      |C Z""".stripMargin

  test("Part 1 examples") {
    assert(strategyScore(parseStrategy(exampleInput)) == 15)
  }

  test("Part 1 input answer") {
    assert(strategyScore(parseStrategy(input)) == 12458)
  }
}
